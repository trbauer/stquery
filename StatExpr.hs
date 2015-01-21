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
  | c >= length cs = StatErr at $ "column " ++ show c ++ " is out of bounds"
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
