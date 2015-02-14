module TestStateExpr where

import StatExpr

import Tim.Test

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

test :: IO ()
test = testParseStatExpr >> testEvalStatExpr >> return ()

q = (\is -> q_pfx ++ ":" ++ intercalate "," (map show is) ++ ":" ++ q_sfx) :: [Int] -> String
q_pfx = "dat/*/*/s$simd*/rgba_to_intensity/4096_4096/$gx*_$gy*/2_1/dat"
q_sfx = "len(#1),@gx*@gy/@simd,1024*avg(#1)/min(#1),1024*med(#1)"
-- full table (default columns)
q1 = q_pfx ++ ":" ++ q_sfx
-- empty merges everything
q2 = q_pfx ++ "::len(#1),1024*avg(#1)/min(#1),1024*med(#1)"
-- permutes
q3 = q [5,4,1,3,2]
-- projects
q4 = q [5,4,3,2]
-- projects more
q5 = q_pfx ++ ":2:len(#1),1024*avg(#1)/min(#1),1024*med(#1)"
-- err (index out of bounds
q6 = q [4,3,2,6]
-- err (index out of bounds
q7 = q [4,3,-2]


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