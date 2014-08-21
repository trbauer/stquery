module StatExpr where

import Control.Applicative((<*),(<$>))
import Data.Char
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

import Tim.ANSI
import Tim.Stats
import Tim.Test


type CellVal = Maybe Double

data StatExpr =
    StatCol !Int !Int -- column projection (a column in a stat file) #1
  | StatRef !Int !String -- a variable reference @ref
  | StatLit !Int !Double -- a constant literal for scaling etc...
  | StatGrp !Int !StatExpr -- a grouped expression: (e)

  | StatPow !Int !StatExpr !StatExpr  -- exponentiation: e^e
  | StatNeg !Int !StatExpr            -- negation: -e

  | StatDiv !Int !StatExpr !StatExpr -- ratio:      e/e
  | StatMul !Int !StatExpr !StatExpr -- product:    e*e
  | StatAdd !Int !StatExpr !StatExpr -- sum:        e + e
  | StatSub !Int !StatExpr !StatExpr -- difference: e - e

  | StatFun !Int !String ![StatExpr] -- a function call

  -- | StatHst !Int !StatExpr   -- histogram
  deriving (Show,Eq)

colStatExpr :: StatExpr -> Int
colStatExpr (StatCol c _) = c
colStatExpr (StatRef c _) = c
colStatExpr (StatLit c _) = c
colStatExpr (StatGrp c _) = c
colStatExpr (StatPow c _ _) = c
colStatExpr (StatNeg c _) = c
colStatExpr (StatDiv c _ _) = c
colStatExpr (StatMul c _ _) = c
colStatExpr (StatAdd c _ _) = c
colStatExpr (StatSub c _ _) = c
colStatExpr (StatFun c _ _) = c

allStatFuncs :: [(String,Int,StatFuncImpl)]
allStatFuncs = [
    ("avg", 1, unaryVS (stAvg . stats)) -- average
  , ("cfv", 1, unaryVS (stCfv . stats)) -- coefficient of variation
  , ("cov", 0, naryVS  cov) -- covariance (needs 2 or more ops)
  , ("len", 1, unaryVS (fromIntegral . length)) -- number of samples (length of a column)
  , ("max", 1, unaryVS maximum)       -- maximum
  , ("med", 1, unaryVS (stMed . stats)) -- median
  , ("min", 1, unaryVS minimum)       -- minimum
  , ("sdv", 1, unaryVS (stSdv . stats)) -- standard deviation (unbiased)
  , ("var", 1, unaryVS (stVar . stats)) -- variance (unbiased)

  , ("sqt", 1, unarySS sqrt)
  , ("log", 1, unarySS (logBase 10))
  , ("lg",  1, unarySS (logBase 2))
  , ("ln",  1, unarySS (logBase (exp 1)))
  , ("exp", 1, unarySS exp)
  ]
  where unaryVS = StatFuncImplUnaVecSca
        naryVS = StatFuncImplNryVecSca
        unarySS = StatFuncImplUnaScaSca

data StatFuncImpl =
    StatFuncImplUnaVecSca ([Double] -> Double)
  | StatFuncImplNryVecSca ([[Double]] -> Double)
  | StatFuncImplUnaScaSca (Double -> Double)

ppStatExpr :: StatExpr -> String
ppStatExpr e =
  case e of
    (StatCol _ i) -> "#" ++ show i
    (StatRef _ r) -> "@" ++ r
    (StatLit _ v) -> show v
    (StatGrp _ e) -> "(" ++ ppStatExpr e ++ ")"
    (StatPow _ e1 e2) -> ppStatExpr e1 ++ "^" ++ ppStatExpr e2
    (StatNeg _ e) -> "-" ++ ppStatExpr e
    (StatDiv _ e1 e2) -> ppStatExpr e1 ++ "/" ++ ppStatExpr e2
    (StatMul _ e1 e2) -> ppStatExpr e1 ++ "*" ++ ppStatExpr e2
    (StatAdd _ e1 e2) -> ppStatExpr e1 ++ " + " ++ ppStatExpr e2
    (StatSub _ e1 e2) -> ppStatExpr e1 ++ " - " ++ ppStatExpr e2
    (StatFun _ f es) -> f ++ "(" ++ intercalate "," (map ppStatExpr es) ++ ")"

data StatValue = StatVec ![Double] -- typically a column projection: #2
               | StatSca !Double   -- a scalar value. e.g. result from a reduction: min($2)
               | StatErr !Int !String -- an error (with an optional column location)
               deriving (Eq,Show)

type Env = String -> Maybe StatValue

-- Evaluates a statistical expression
evalStatExpr :: [[CellVal]] -> Env -> StatExpr -> StatValue
evalStatExpr _  _ (StatLit _ v) = StatSca v
evalStatExpr cs _ (StatCol at c)
  | c > length cs = StatErr at $ "column " ++ show c ++ " is out of bounds"
  | any isNothing cvals = StatErr at $ "column " ++ show c ++ " contains malformed data"
  | otherwise = StatVec (map (\(Just v) -> v) cvals)
  where cvals = cs !! c
        isNothing Nothing = True
        isNothing _ = False
evalStatExpr cs env (StatRef c var) = fromMaybe (StatErr c $ "unbound identifier " ++ var) (env var)

evalStatExpr cs env (StatGrp _ e) = evalStatExpr cs env e
evalStatExpr cs env (StatNeg _ e) = evalStatExprUn negate cs env e
evalStatExpr cs env (StatPow _ e1 e2) = evalStatExprBin (**) cs env e1 e2
evalStatExpr cs env (StatMul _ e1 e2) = evalStatExprBin (*) cs env e1 e2
evalStatExpr cs env (StatDiv _ e1 e2) = evalStatExprBin (/) cs env e1 e2
evalStatExpr cs env (StatAdd _ e1 e2) = evalStatExprBin (+) cs env e1 e2
evalStatExpr cs env (StatSub _ e1 e2) = evalStatExprBin (-) cs env e1 e2
evalStatExpr cs env (StatFun c f es) =
  case find (\(g,_,_) -> g == f) allStatFuncs of
    Nothing -> error $ show c ++ ". cannot find function in allStatFuncs for " ++ f
    Just (nm,n,impl)
      | n /= 0 && n /= length es -> StatErr c $ "wrong number of arguments to " ++ nm
      | otherwise -> evalStatExprFunc impl c nm cs (map (evalStatExpr cs env) es)
-- evalStatExpr _ _ e = StatErr 0 $ "implement: " ++ show e

evalStatExprBin :: (Double -> Double -> Double) -> [[CellVal]] -> Env -> StatExpr -> StatExpr -> StatValue
evalStatExprBin op cs env e1 e2 =
  case (evalStatExpr cs env e1, evalStatExpr cs env e2) of
    (e@(StatErr _ _), _) -> e
    (_, e@(StatErr _ _)) -> e
    (StatSca v1, StatSca v2) -> StatSca (v1 `op` v2)
    (StatSca v1, StatVec v2) -> StatVec (map (v1`op`) v2)
    (StatVec v1, StatSca v2) -> StatVec (map (`op`v2) v1)
    (StatVec v1, StatVec v2) -> StatVec (zipWith op v1 v2)

evalStatExprUn :: (Double -> Double) -> [[CellVal]] -> Env -> StatExpr -> StatValue
evalStatExprUn op cs env e =
  case evalStatExpr cs env e of
    e@(StatErr _ _) -> e
    StatSca v -> StatSca (op v)
    StatVec vs -> StatVec (map op vs)

evalStatExprFunc :: StatFuncImpl -> Int -> String -> [[CellVal]] -> [StatValue] -> StatValue
evalStatExprFunc impl col nm cs vs
  | not (null errs) = head errs
  | otherwise = case impl of
                  StatFuncImplUnaVecSca f ->
                    case head vs of
                      e@(StatErr _ _) -> e
                      StatSca _ -> StatErr col $ "scalar passed to " ++ nm
                      StatVec ds -> StatSca (f ds)
                  StatFuncImplNryVecSca f
                    | not (null scalars) -> StatErr col $ "arg " ++ show (fst (head scalars)) ++
                                                          " scalar passed to " ++ nm
                    | otherwise -> StatSca $ f (map (\(StatVec ds) -> ds) vs)
                  StatFuncImplUnaScaSca f
                    | length vs /= 1 -> StatErr col $ nm ++ " requires exactly one argument"
                    | otherwise -> case head vs of
                                     StatSca v -> StatSca (f v)
                                     StatVec ds -> StatVec (map f ds)
  where errs = filter isErr vs
        isErr (StatErr _ _) = True
        isErr _ = False
        scalars = filter (isSca . snd) (zip [1..] vs)
        isSca (StatSca _) = True
        isSca _ = False

--
-- > PROPOSED GRAMMAR (some features may not be implemented):
-- >    <spec>      ::= list(<col_expr>,',')
-- >    <col_expr>  ::=
-- >                    <lit>
-- >                  | '#' [0-9]+                 -- column reference
-- >                  | '@' [0-9]+                 -- variable reference
-- >                  | <col_expr> '+' <col_expr>
-- >                  | <col_expr> '-' <col_expr>
-- >                  | <col_expr> '*' <col_expr>
-- >                  | <col_expr> '/' <col_expr>
-- >                  | '-' <col_expr>
-- >                  | <col_expr> '^' <col_expr>
-- >                  | '(' <col_expr> ')'
-- >                  | <sca_func>  '(' <col_expr> ')'
-- >                  | <agr_func>  '(' <col_ref> ')'
-- >                  | <agr2_func> '(' <col_ref> ',' <col_ref> ')'
-- >    <sca_func>  ::= -- scalar functions
-- >                    'cos'
-- >                  | 'log'   -- log-base 10
-- >                  | 'exp'
-- >                  | 'lg'    -- log-base 2
-- >                  | 'ln'    -- natural logarithm
-- >                  | 'sin'
-- >                  | 'sqt'
-- >                  | 'tan'
-- >    <agr_func>  ::= -- aggregation functions
-- >                    'avg'   -- average
-- >                  | 'cfv'   -- coefficient of variation
-- >                  | 'len'   -- length of the column
-- >                  | 'max'   -- maximum in the column
-- >                  | 'med'   -- median
-- >                  | 'min'   -- minimum
-- >                  | 'sdv'   -- standard deviation (unbiased)
-- >                  | 'sum'   -- column sum
-- >    <agr2_func> ::= -- binary aggregion functions
-- >                    'cov'   -- covariance
-- >    <lit>       ::= (read :: String -> Double)
parseStatExprs :: String -> Either String [StatExpr]
parseStatExprs inp =
  case parse (pExprs <* eof) "" inp of
    Left err -> Left $ errToStr err
    Right r -> Right r
  where errToStr :: ParseError -> String
        errToStr err = show ecol ++ ". " ++ dropWhile isSpace emsg ++ "\n" -- show ecol ++ ". " ++ emsg ++ "\n"
                          ++ inp ++ "\n"
                          ++ replicate (ecol-1) ' ' ++ "^\n"
            where ecol = sourceColumn (errorPos err)
                  -- emsg =  intercalate "; " (map messageString (errorMessages err))
                  emsg = drop 1 $ dropWhile (/=';') (concatMap (\c -> if c == '\n' then "; " else [c]) (show err))


def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "-*/+^"
              , opLetter = oneOf ""
              , reservedOpNames = ["-", "*", "/", "+", "-"]
              , reservedNames = fnames
              }
              where fnames = map (\(nm,_,_) -> nm) allStatFuncs


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , commaSep = m_commaSep
           , lexeme = m_lexeme
           , symbol = m_symbol
           , whiteSpace = m_whiteSpace } = makeTokenParser def

pApplyCol :: Parser (Int -> StatExpr) -> Parser StatExpr
pApplyCol p = do
  m_whiteSpace
  cl <- fmap sourceColumn getPosition
  fmap (\pF -> pF cl) p

pExprs :: Parser [StatExpr]
pExprs = sepBy pExpr (m_symbol ",")

pExpr :: Parser StatExpr
pExpr = pApplyCol expr
 where expr :: Parser (Int -> StatExpr)
       expr = buildExpressionParser table term <?> "expression"

       term :: Parser (Int -> StatExpr)
       term = do
        t <- pTerm
        return (const t)

        -- Table returns: Parser (Int -> StatExpr)
       table :: OperatorTable String () Identity (Int -> StatExpr)
       table = [
            [binOpR "^" StatPow]
          , [unOp   "-" StatNeg]
          , [binOpL "*" StatMul, binOpL "/" StatDiv]
          , [binOpL "+" StatAdd, binOpL "-" StatSub]
        ]

       binOpL, binOpR :: String -> (Int -> StatExpr -> StatExpr -> StatExpr) -> Operator String () Identity (Int -> StatExpr)
       binOpL s cons = Infix (m_reservedOp s >> return (\e1 e2 c -> cons c (e1 c) (e2 c))) AssocLeft
       binOpR s cons = Infix (m_reservedOp s >> return (\e1 e2 c -> cons c (e1 c) (e2 c))) AssocRight

       unOp :: String -> (Int -> StatExpr -> StatExpr) -> Operator String () Identity (Int -> StatExpr)
       unOp s cons = Prefix $ m_reservedOp s >> return (\e c -> cons c (e c))

pTerm :: Parser StatExpr
pTerm = m_lexeme (pApplyCol $
                       pGroup
                   <|> pCol
                   <|> pRef
                   <|> pFuncOrLiteral)
  where pGroup = do { m_symbol "("; e <- pExpr; m_symbol ")"; return (`StatGrp` e) }
        pCol = do { char '#'; ds <- many1 digit; return (`StatCol` read ds) }
        pRef = do { char '@'; ref <- pRefId; return (`StatRef` ref) }
        pRefId = do { c <- letter <|> char '_'; cs<-many alphaNum; return (c:cs) }
               <|> many1 digit

pFuncOrLiteral :: Parser (Int -> StatExpr)
pFuncOrLiteral = tryFunc allStatFuncs
  where tryFunc ((fnm,arity,_):rest) =
              try (do { m_reserved fnm;
                        m_symbol "(";
                        es <- m_commaSep pExpr;
                        if arity > 0 && arity /= length es then
                          fail (fnm ++ " takes " ++ show arity ++ " argument" ++ if arity > 1 then "s" else "")
                          else do
                            m_symbol ")";
                            return (\c -> StatFun c fnm es) })
          <|> tryFunc rest
        tryFunc [] = pLit

pLit :: Parser (Int -> StatExpr)
pLit = do
  ds1 <- many1 digit
  ds2 <- option "" $ do
    char '.'
    ds2 <- many1 digit
    return ("." ++ ds2)
  return (\c -> StatLit c (read (ds1 ++ ds2)))

testP :: Show a => Parser a -> String -> IO ()
testP p inp = case parse (p <* eof) "" inp of
             { Left err -> print err
             ; Right r -> print r
             }

-- Also tests the pretty printer
testParseStatExpr :: IO Int
testParseStatExpr = fst <$> runTests parse allTests
  where allTests = [
            oky "" []
          , oky "#21" [StatCol 1 21]
          , oky "@21,@abc,@abc21,@_" [StatRef 1 "21",StatRef 5 "abc",StatRef 10 "abc21",StatRef 17 "_"]
          , err "@2a" "3. unexpected 'a'"
          , oky "#1,#2" [StatCol 1 1,StatCol 4 2]
          , oky "(#21)" [StatGrp 1 (StatCol 2 21)]
          , err "#" "2. unexpected end of input"
          , err "#a" "2. unexpected \"a\""
          , oky "232" [StatLit 1 232]
          , oky "232.22" [StatLit 1 232.22]
          , err "232." "5. unexpected end of input"
          , err ".2" "1. unexpected '.'"

          , oky "#1,44" [StatCol 1 1, StatLit 4 44]
          , oky "#1, 44" [StatCol 1 1, StatLit 5 44]
          , oky "#1 ,44" [StatCol 1 1, StatLit 5 44]
          , oky "#1+#2" [StatAdd 1 (StatCol 1 1) (StatCol 4 2)]
          , oky "#1+#2-#3*#4/#5" [StatSub 1
                                    (StatAdd 1 (StatCol 1 1) (StatCol 4 2))
                                    (StatDiv 1 (StatMul 1 (StatCol 7 3) (StatCol 10 4))
                                               (StatCol 13 5))]
          , oky "-#1- -#2" [StatSub 1 (StatNeg 1 (StatCol 2 1)) (StatNeg 1 (StatCol 7 2))]
          , oky "-#1--#2" [StatSub 1 (StatNeg 1 (StatCol 2 1)) (StatNeg 1 (StatCol 6 2))]
          , oky "1^2^3" [StatPow 1 (StatLit 1 1.0) (StatPow 1 (StatLit 3 2.0) (StatLit 5 3.0))]
          , oky " 1 + 2 " [StatAdd 2 (StatLit 2 1.0) (StatLit 6 2.0)]
          , oky "avg(#3)" [StatFun 1 "avg" [StatCol 5 3]]
          , oky "sqt(1) + log(2)" [StatAdd 1 (StatFun 1 "sqt" [StatLit 5 1.0]) (StatFun 10 "log" [StatLit 14 2.0])]
          , oky "ln(1) + exp(1)" [StatAdd 1 (StatFun 1 "ln" [StatLit 4 1.0]) (StatFun 9 "exp" [StatLit 13 1.0])]
          , oky "med ( 1024*#3 ) " [StatFun 1 "med" [StatMul 7 (StatLit 7 1024.0) (StatCol 12 3)]]
          , oky "cov(#1,#2) " [StatFun 1 "cov" [StatCol 5 1,StatCol 8 2]]
          , err "med(#1,#2)" "med takes 1 argument"
          ]

        parse :: String -> Either String [StatExpr]
        parse s = do
           es <- parseStatExprs s
           let ppR = normalize (intercalate "," (map ppStatExpr es))
               ppS = normalize s
               -- attempts to normalize pp output
               normalize = fixInts . despace

               despace = concatMap (\c -> if isSpace c then [] else [c])

               fixInts [] = [] -- changes 3.0+4.0 -> "3+4"
               fixInts ".0" = []
               fixInts ('.':'0':c:cs)
                 | isDigit c = '.' : '0' : fixInts (c:cs)
                 | otherwise = fixInts (c:cs)
               fixInts (c:cs) = c : fixInts cs
           if ppR /= ppS then Left $ "despaced pretty print failed: " ++ show ppR ++ " (expected " ++ show ppS ++ ")"
             else return es

testEvalStatExpr = runTests eval tests
  where env v = if v == "foo" then Just (StatSca 3) else Nothing
        mkCells = map (map Just)
        ds = [[2,3],[2,2,3,4],[4,3]] :: [[Double]]
        cs = mkCells ds :: [[CellVal]]

        tests = [
            oky "44.1"        (StatSca 44.1)
          , oky "#0"          (StatVec [2,3])
          , oky "@foo"        (StatSca 3)
          , oky "(@foo)"      (StatSca 3)
          , oky "2^3^2"       (StatSca 512)
          , oky "-2"          (StatSca (-2))
          , oky "2*#0"        (StatVec [4,6])
          , oky "6/3*2-2-1+1" (StatSca 2)
          , oky "len(#1)"     (StatSca 4)
          , oky "len(#1)"     (StatSca 4)
          ] ++ map makeCallCase allStatFuncs ++
          [
            oky "@err"        (StatErr 1 "unbound identifier err")
          , oky "len(@foo)"   (StatErr 1 "scalar passed to len")
          ]

        makeCallCase fun =
          case fun of
            (fnm,1,fimpl) -> case fimpl of
                               StatFuncImplUnaVecSca f -> oky (fnm++"(#0)") (StatSca (f (head ds)))
                               StatFuncImplUnaScaSca f -> oky (fnm++"(#0)") (StatVec (map f (head ds)))
                               _ -> err (fnm ++ ": StatFuncImpl type not supported") ""
            (fnm,0,fimpl) -> case fimpl of
                               StatFuncImplNryVecSca f -> oky (fnm++"(#0,#2)") (StatSca (f [head ds, ds !! 2]))
                               _ -> err (fnm ++ ": StatFuncImpl type not supported") ""
            _ -> err "arity not supported" ""

        eval :: String -> Either String StatValue
        eval s = case parseStatExprs s of
                   Left err -> Left err
                   Right [e] -> Right $ evalStatExpr cs env e
                   Right _ -> Left "parsed multiple expressions"