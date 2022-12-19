module Lib
( work ) where

import System.IO
import GHC.Read
import Text.ParserCombinators.ReadPrec (lift,(<++))
import Text.ParserCombinators.ReadP (skipSpaces,skipMany,skipMany1,ReadP,get,char,choice)
 
mnk :: [Double] -> [Double] -> (Double,Double,Double)
mnk x y = (a,b,sigm)
          where n    = length x
                sx   = sum x
                sy   = sum y
                sx2  = sum $ map (^(2 :: Int)) x
                sxy  = sum $ zipWith (*) x y
                det  = sx2*(fromIntegral n)-sx*sx
                deta = (fromIntegral n)*sxy-sx*sy
                detb = sx2*sy-sx*sxy
                a    = deta/det
                b    = detb/det
                sigm = sum $ zipWith (\xi yi -> (yi - (a*xi+b))^(2 :: Int)) x y

exponential :: [Double] -> [Double] -> (Double,Double,Double)
exponential x y = (a,b,sigm)
          where n     = length x
                sx    = sum x
                sy    = sum $ map log y
                sx2   = sum $ map (^(2 :: Int)) x
                sxy   = sum $ zipWith (*) x (map log y)
                a     = (sy*sx2 - sx*sxy) / ((fromIntegral n)*sx2 - sx*sx)
                b     = ((fromIntegral n)*sxy - sy*sx) / ((fromIntegral n)*sx2 - sx*sx)
                sigm  = sum $ zipWith (\xi yi -> ((exp a)*(exp (b * xi)) - yi)^(2 :: Int)) x y
                                 
                                 
data InputData = XY Double Double | Complete

oneOf :: [Char] -> ReadP Char
oneOf cs = choice [char c | c <- cs]
 
instance GHC.Read.Read InputData where
 
        readPrec = (do
                        x <- readPrec
                        lift $ skipMany1 (oneOf ";")
                        y <- readPrec
                        return (XY x y))
 
                     <++ (do
                        lift $ skipMany get
                        return Complete)
 
chOption 1 xs ys = do
                let (a,b,sigm) = mnk xs ys
                showans a b sigm
chOption 2 xs ys = do
                let (a,b,sigm) = exponential xs ys
                showans a b sigm
chOption _ xs ys = putStr $ "Error choosing option, cancelling..."

showans a b sigm = do
                putStrLn $ "\na=" ++ show a
                putStrLn $ "b=" ++ show b
                putStrLn $ "sigm=" ++ show sigm

work :: IO ()
work = do
    hSetBuffering stdout NoBuffering
    let dataInput i xs ys = do
            let strI = show i
            putStr $ "Enter x" ++ strI ++ " y" ++ strI ++ 
                        " or not a number to complete : " 
            eof <- isEOF
            case eof of
                True -> return (xs, ys)
                False -> do
                            inp <- readLn
                            case inp of
                                (XY x y) -> dataInput (i+1) (xs++[x]) (ys++[y])
                                Complete -> return (xs,ys)
    (xs,ys) <- dataInput (0 :: Int) [] []
    if length xs < 2 then putStrLn "no data"
    else do putStr $ "Choose approx: " ++ "1.lineal; " ++ 
                        "2.exponential : " 
            inp <- readLn
            chOption inp xs ys