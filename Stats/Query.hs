module Stats.Query where

import Stats.Expr
import Stats.Util

import Control.Applicative
import Control.Arrow
import Control.Exception hiding (try)
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.IO
import Text.Printf

import Text.Escapes.ANSI -- ansi-color
import Text.Glob -- globexpr

-- The number of significant and fractional digits to use in the
-- output values. E.g. 4 and 5 gives: SSSS.FFFFF or "%10.5f".
mIN_SIGNIFICANTS = 4 :: Int
mIN_FRACTIONS = 6 :: Int


-- TODO:
--   * Labeled query expressions.
--           "runtime"@med(#1/#0)
--           expr as "average runtime"
--           expr:runtime
--           expr:"average runtime"
--
--   * Deal with dependent variables:  domain inference
--       * (fix checkTableWidth) (concat all vars . nub) keep this as part of the keys
--         have to reconcile this with the permutation op?
--       * have to augment defs for all rows (insert missing defs and initially order
--         them with respect to the globs)
--
--   * Generalize table? From [[CellVal]] to [[DatVal]]
--                         data DatVal = DatDbl !Double | DatStr !String | DatDate !Date
--
--   * Support symbolic information (histograms).
--      * SPECIFY: do we support histograms?
--
--   * Define mode(#1)
--
--   * Define joins. ':' #1:#2 joins two columns.
--
--   * Define list literals. [Why?]
--
--   * Support gzipp'ed archives for glob?
--
-- STORYBOARD:
--  a  x foo   ... data
--       bar   ... data
--       qux   ... data
--  a  y foo   ... data
--       bar   ... data
--       qux   ... data
--  b  x foo   ... data
--       bar   b/x/=>bar/dat not found
--       qux   b/x/qux/dat: 3.22. malformed number
--  b  y foo   ... data
--       bar   ... data
--       qux   ... data
putErrLn msg = hPutStrLnANSI stderr (aNSI_RED ++ msg ++ aNSI_RESET)
fatal msg = putErrLn msg >> exitFailure

runStr :: Verbosity -> String -> IO ()
runStr verbose str = do
  whenVerbose verbose $
    putStrLn (str++":")
  case splitOn (==':') str of
    [gstr,sexpr] -> runStrs verbose Nothing gstr sexpr
    [gstr,istr,sexpr] -> case reads ("[" ++ istr ++ "]") :: [([Int],String)] of
                           [(is,"")] -> runStrs verbose (Just is) gstr sexpr
                           _ -> putErrLn $ "malformed glob variable ordering list: " ++ istr
    _ -> putErrLn $ "malformed query string: " ++ str

runStrs :: Verbosity -> Maybe [Int] -> String -> String -> IO ()
runStrs verbose mis gstr estr =
  case (parseGlob gstr, parseStatExprs estr) of
    (Left e, _) -> putErrLn $ "malformed glob expression: " ++ e
    (_, Left e) -> putErrLn $ "malformed stat column expression: " ++ e
    (Right g, Right es) -> do
      -- whenVerbose verbose $ do
      --  putStrLn $ "  " ++ show g
      --  putStrLn $ "  " ++ show mis
      --  putStrLn $ "  " ++ show es
      fs <- expandGlobPath g "."
      if null fs then putErrLn "no glob matches (try -l)"
--        part <- walkPartialGlobPath "." g
--        hPutStrLn stderr $ part
        else do
          let nvars = length (map fst (fst (head fs)))
          case mis of
            Nothing -> readAndPrintTable verbose [1 .. nvars] fs es
            Just is -> do let oob = filter (\i -> i > nvars || i < 1) is
                          if not (null oob) then putErrLn $ "projection index(ices) are out of bounds: " ++ show oob
                            else readAndPrintTable verbose is fs es

-- walkPartialGlobPath :: FilePath -> Glob -> IO String
-- walkPartialGlobPath fp (GlobSeq _ gs) = walkPathChunks fp (splitWhen isSep gs)
--  where isSep (GlobSeq _) = True
--
--        -- .../pfx{a,b}_{c,d}sfx/...
--        -- [[...],[lit pfx,alt a b,lit _, alt{c,d}, lit sfx],[...]
--        walkPathChunks :: FilePath -> [[Glob]] -> IO String
--        walkPathChunks fp [] = return []
--        walkPathChunks fp (g:gs) = walkPathChunk g
--
--        walkPathChunk :: FilePath -> [Glob] -> IO FilePath
--        walkPathChunk
--
-- walkPartialGlobPath _ _ = return ""

readAndPrintTable :: Verbosity -> [Int] -> [(VarDefs,FilePath)] -> [StatExpr] -> IO ()
readAndPrintTable v is fs es = do
  tbl <- readTable fs
  let pipeline = formatTable v es . evaluateTable es . permuteTable is . checkTableWidth
  hPutStrANSI stdout (pipeline tbl)

--
data Verbosity = VerbNone | VerbSome | VerbAll deriving (Eq,Show)
whenVerbose VerbNone _ = return ()
whenVerbose _ io = io

-- type Table a = [([String],a)]
type Table a = [([(String,String)],Row a)]
-- type Row = ([String],RowVal)
-- Left is error, Right is ([Warning],[(File,Contents)],Value)
type Row a = Either String ([String],[(FilePath,String)],a)
-- type RowVal = Either String ([(FilePath,String)],[Double])

mkRow :: [(String,String)] -> [(FilePath,String)] -> a -> ([(String,String)],Row a)
mkRow vs fs a = (vs, Right ([], fs, a))
mkErrorRow :: [(String,String)] -> String -> ([(String,String)],Row a)
mkErrorRow vs err = (vs, Left err)

readTable :: [(VarDefs,FilePath)] -> IO (Table [[CellVal]])
readTable fs =
  forM fs $ \(defs,fp) -> do
      let returnErrRow e = return (defs, Left (fp ++ ": " ++ e))
      is_dir <- doesDirectoryExist fp
      if is_dir then returnErrRow "is a directory"
        else do
          fstr <- readFile fp
          length fstr `seq` return ()
          let raw_dat = parseData fstr
          return $ mkRow defs [(fp,fstr)] (transposeWithFill Nothing raw_dat)

-- TODO: come up with something more reasonable.
-- This isn't a problem unless they used nested variables.
-- {a,{b,c}} gives 1 or 2 columns.  We need to create a column for each.
-- This should have 2 columns with a "" filled in if a is chosen?
checkTableWidth :: Table a -> Table a
checkTableWidth (r:rs) = r : map checkElem rs
  where nVars = length (fst r)
        checkElem (vs,a)
          | length vs /= nVars = (vs,Left "ragged table row")
          | otherwise = (vs,a)
-- checkTableWidth (r:rs) = map
--  where rPadded = map (first (padAs dfts)) t
--
--        maxVars = maximum $ (map (length . fst) rs)
--        dfts = replicate maxVars ""
--
--        padAs [d]    (a:as) = a : padAs [d] as
--        padAs (_:ds) (a:as) = a : padAs ds  as
--        padAs (d:ds) []     = d : padAs ds  []

permuteTable :: [Int] -> Table [[CellVal]] -> Table [[CellVal]]
permuteTable is = pipeline
  where -- 1. Permute and project out invalid columns (this columns that might have equal keys)
        -- 2. Factor the duplicates and join them
        -- 3. Join the common keys
        pipeline :: Table [[CellVal]] -> Table [[CellVal]]
        pipeline = map (second mergeRows) . relFactor . relPermute is

        mergeRows :: [Row [[CellVal]]] -> Row [[CellVal]]
        mergeRows [] = Right ([],[],[])
        mergeRows (Left e : _) = Left e
        mergeRows (Right (ws1,fs1,a1) : rs) =
          case mergeRows rs of
            Left e -> Left e
            Right (ws2,fs2,a2) -> Right (ws1 ++ ws2, fs1 ++ fs2, mergeCols a1 a2)

        -- Note: this permits ragged columns.  We are okay with this though
        mergeCols :: [[CellVal]] -> [[CellVal]] -> [[CellVal]]
        mergeCols [] [] = []
        mergeCols (c1:cs1) (c2:cs2) = (c1 ++ c2) : mergeCols cs1 cs2
        mergeCols (c1:cs1) []       = c1 : mergeCols cs1 []
        mergeCols []       (c2:cs2) = c2 : mergeCols [] cs2

evaluateTable :: [StatExpr] -> Table [[CellVal]] -> Table [Either String Double]
evaluateTable es = map evalRow
  where evalRow (vs,Left err) = (vs,Left err)
        evalRow (vs,Right (ws,fs,cs)) = (vs,Right (ws,fs,evalCells cs env))
          where env :: String -> Maybe StatValue
                env var = do
                  val <- lookup var vs
                  case reads val :: [(Double,String)] of
                    [(d,"")] -> return $! StatSca d
                    _ -> fail "no bindings"

        evalCells :: [[CellVal]] -> Env -> [Either String Double]
        evalCells cs env = map (evalCell cs env) es

        evalCell :: [[CellVal]] -> Env -> StatExpr -> Either String Double
        evalCell cs env e = case evalStatExpr cs env e of
                              StatSca v   -> Right v
                              StatVec _   -> Left "evaluates to a vector"
                              StatErr c e -> Left (show c ++ ". " ++ e)

formatTable :: Verbosity -> [StatExpr] -> Table [Either String Double] -> String
formatTable verbosity es tbl = aNSI_WHI ++ header_row ++ data_rows ++ aNSI_RESET
  where ks :: [String]
        ks = nub (concatMap (map fst . fst) tbl)

        keylen :: String -> [(String,String)] -> Int
        keylen k vs = case lookup k vs of
                        Just v -> length v
                        Nothing -> 0

        maxlen k = maximum $ map (\(vs,_) -> keylen k vs) tbl

        ks_len = map (\k -> max (length k) (maxlen k)) ks

        ks_fmts :: [String -> String]
        ks_fmts = map (\w s -> printf ("%" ++ show w ++ "s") s) ks_len

        ks_header = intercalate "  " (zipWith ($) ks_fmts ks)
        v_strs = map (padL (mIN_SIGNIFICANTS + 1 + mIN_FRACTIONS) . ppStatExpr) es
        vs_header = intercalate "  " v_strs
        header_row = ks_header ++ " | " ++ vs_header ++ "\n"
        padL n s = replicate (n - length s) ' ' ++ s

        -- number of value columns (at least 1)
        vcols :: Int
        vcols = length es

        -- vals i = concatMap (vs . (!! i)) t
        --    where vs (Left _) = []
        --          vs (Right (_,_,v)) = [v]
        -- TODO: infer float width specifier (precision)
        v_fmts :: [Either String Double -> String]
        v_fmts = map (fmt . length) v_strs
          where fmt :: Int -> Either String Double -> String
                -- fmt len (Left err) = printf ("%" ++ show len ++ "s") err
                -- fmt len (Right d) = printf ("%" ++ show len ++ ".5f") d
                fmt len esd = case esd of
                                Left err -> printf ("%" ++ show len ++ "s") err
                                Right d -> printf ("%" ++ show len ++ "."++show mIN_FRACTIONS++"f") d

        -- ppRows :: [Row [Either String Double]] -> String
        ppRows [] = []
        ppRows rs = ppRowsN [] rs
        ppRowsN _     []              = []
        ppRowsN pv_ks ((defs,val):rs) = ks_str ++ " | " ++ v_str ++ "\n" ++ warning_lines ++ verbose_lines  ++ ppRowsN ks rs
           where ks = map snd defs
                 ks_str = ppKeys ks_fmts pv_ks ks
                 v_str = ppVals val
                 onRight f = case val of
                               (Right (ws,fs,_)) -> f ws fs
                               (Left _) -> ""
                 warning_lines
                  | verbosity == VerbNone = ""
                  | otherwise = onRight $ \ws _ -> aNSI_YEL ++ unlines ws ++ aNSI_WHI
                 verbose_lines
                  | verbosity == VerbNone = ""
                  | otherwise = onRight $ \_ fs -> aNSI_DK_WHI ++ concatMap ppFile fs ++ aNSI_WHI
                  where ppFile (f,fstr) = "[" ++ f ++ "]" ++ if verbosity == VerbSome then "\n" else "\n" ++ fstr

        -- ps is the previous row's keys, we use these to avoid
        -- redundant printing.
        --
        -- if ps == [] then we print all keys
        ppKeys :: [String -> String] -> [String] -> [String] -> String
        ppKeys fmts ps ks = intercalate "  " $ ppKeysN fmts ps ks
        ppKeysN [] [] [] = []
        ppKeysN (fmt:fmts) []     (k:ks) = fmt k : ppKeysN fmts [] ks
        ppKeysN (fmt:fmts) (p:ps) (k:ks)
          | p == k = fmt "" : ppKeysN fmts ps ks
          | otherwise = fmt k : ppKeysN fmts [] ks

        ppVals :: Row [Either String Double] -> String
        ppVals (Left err) = err
        ppVals (Right (_,_,vs)) = intercalate "  " $ zipWith ($) v_fmts vs

        data_rows = ppRows tbl

parseData :: String -> [[CellVal]]
parseData = readLines [] . lines
  where readLines vs [] = reverse vs
        readLines vs (ln:lns)
          | null trmln        = readLines vs lns
          | head trmln == '#' = readLines vs lns
          | otherwise         = readLines (v:vs) lns
          where trmln = dropWhile isSpace ln
                trcln = takeWhile (/='#') trmln
                toMaybeDouble [(d,"")] = Just d
                toMaybeDouble _ = Nothing
                v = map (toMaybeDouble . reads) (words trcln)
