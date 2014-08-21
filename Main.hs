module Main where

import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import StatQuery

-- The version table
--   * 1.0.1 added version
--   * 1.0.2 added -l option
vERSION = "1.0.2"

main = do
  opts <- getArgs >>= parseArgs defaultOpts
  when (oHelp opts) $ do
    putStrLn usage
    exitSuccess
  when (oListTree opts) $ do
    listTree "."
    exitSuccess
  when (null (oQueries opts)) $
    putStrLn usage
  mapM_ (runStr (oVerbose opts)) (oQueries opts)
  exitSuccess

data Opts = Opts {
    oVerbose :: !Verbosity
  , oQueries :: [String]
  , oListTree :: !Bool
  , oHelp :: !Bool
  } deriving Show

defaultOpts = Opts VerbNone [] False False

usage :: String
usage =
  "usage: stquery.exe (-h|-l|-v|-V) <query>+\n" ++
  "where\n" ++
  "  -h          prints this message\n" ++
  "  -l          lists the directory tree to query\n" ++
  "  -v          enables verbose output\n" ++
  "  -V          enables more verbose output\n" ++
  "  --version   prints the version and exits\n" ++
  "\n" ++
  "  <query> is a query string consisting of a glob expression, a possible\n" ++
  " column permutation, and statistical column information\n" ++
  "\n" ++
  "  <query> ::= <globexpr>:<colspec>\n" ++
  "            | <globexpr>:<int>+:<colspec>\n" ++
  "  <globexpr> is a glob expression\n" ++
  "             - * matchs one or more characters\n" ++
  "             - ? matches exactly one character\n" ++
  "             - {a,b} alternates between 'a' and 'b'\n" ++
  "             - matches can be named \"$v{a,b}\" binds \"v\" the match\n" ++
  "             - @foo attempts to parse the value of named glob variable @foo\n" ++
  "               as a number.\n" ++
  "  <int>      is a list of columns; this is the column permutation indicates\n" ++
  "             which order to list glob variables (variables are indexed\n" ++
  "             [1 ... n])\n" ++
  "  <colspec> is a column specifier\n" ++
  "             - $n is the n'th column of the file\n" ++
  "             - functions like 'avg', 'max', and 'med' reduce columns to scalars\n" ++
  "             - numeric literals and arithmetic are permitted (e.g. 1024.0*$2)\n" ++
  "             - operations on columns is pointwise\n" ++
  "             - operations on a column and a scalar are a replicated-scalar view\n" ++
  "\n" ++
  "FULL EXAMPLES:\n" ++
  "  dat/*/*/foo/4096_4096/{foo,bar}/dat:avg(#0),2*sdv(#0)\n" ++
  "    expands glob \"dat/*/*/foo/4096_4096/{foo,bar}/dat\" and lists the average\n" ++
  "    of the first column of those files \"avg(@0)\" and twice the standard\n" ++
  "    deviation of the same data.  The captured glob variable columns are\n" ++
  "    automatically ordered left-to-right starting with the first * up to the\n" ++
  "    last (the \"{foo,bar}\" alternation).  The data file is expected to be in a\n" ++
  "    space-deliminted table of numbers with possible end-of-line comments\n" ++
  "    starting with '#'.\n" ++
  "\n" ++
  "  dat/*/*/*/*/dat:4,2,3,1:med(#1*4096)\n" ++
  "    expands glob \"dat/*/*/*/*/dat:4,1,3,2\" and lists the median of the\n" ++
  "    second column with each element multipled by 4096 \"med(#1*4096)\"\n"


parseArgs :: Opts -> [String] -> IO Opts
parseArgs opts [] = return opts
parseArgs opts ("-v":as) = parseArgs (opts{oVerbose = VerbSome}) as
parseArgs opts ("-V":as) = parseArgs (opts{oVerbose = VerbAll}) as
parseArgs opts ("-h":as) = parseArgs (opts{oHelp = True}) as
parseArgs opts ("-l":as) = parseArgs (opts{oListTree = True}) as
parseArgs opts ("--version":as) = putStrLn vERSION >> exitSuccess
parseArgs opts (o@('-':_):as) = fatal $ "unrecognized option: " ++ o ++ "\n" ++ usage
parseArgs opts (a:as) =  parseArgs (opts{oQueries = oQueries opts ++ [a]}) as


getPaths :: FilePath -> IO [FilePath]
getPaths dir = filter ((/='.') . head) <$> getDirectoryContents dir

getAllPaths :: [FilePath] -> IO [FilePath]
getAllPaths fps = nub <$> concatMapM getPaths fps

concatMapM :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f as = concat <$> mapM f as

listTree :: FilePath -> IO ()
listTree dir = do
  putStrLn "Query Tree:"
  listTreeRec [dir] >>= putStrLn

listTreeRec :: [FilePath] -> IO String
listTreeRec [] = return ""
listTreeRec dirs = do
  fs <- getAllPaths dirs :: IO [FilePath]
  let ps :: [FilePath]
      ps = concatMap (\dir -> map (\f -> dir </> f) fs) dirs
  ds <- filterM doesDirectoryExist ps
  sfx <- listTreeRec ds
  let str = if length fs > 1 then "{" ++ intercalate "," fs ++ "}" else head fs
  return $ "    " ++ str ++ "\n" ++ sfx


