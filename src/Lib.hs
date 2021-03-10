module Lib where

import           Data.Bits
import           Data.List

data Term = One | Zero | DC deriving (Eq)

data Cube = Cube {noOfTerms :: Int
                 ,terms     :: [Term]
                 ,minTerms  :: [Int]} deriving (Show)

instance Show Term where
    show One  = "1"
    show Zero = "0"
    show DC   = "-"


instance Eq Cube where
    a == b = terms a == terms b

findTerms :: Int -> [Term]
findTerms 1 = [One]
findTerms 0 = [Zero]
findTerms a
    | a `mod` 2 == 1 = One:findTerms (a `shiftR` 1)
    | otherwise      = Zero:findTerms (a `shiftR` 1)


findTermsPadded :: Int -> Int -> [Term]
findTermsPadded mt cnt = replicate (cnt - fixedLen) Zero ++ reverse fixedTerms
    where fixedTerms = findTerms mt
          fixedLen = length fixedTerms

unitCube :: Int -> Int -> Cube
unitCube mt cnt
    | length thisTerms == cnt = Cube {noOfTerms = cnt
                                          ,terms = thisTerms
                                          ,minTerms = [mt]}
    | otherwise = error $ "Cannot create cube from " ++ show mt ++ " to have " ++ show cnt ++ " terms"
    where
        thisTerms = findTermsPadded mt cnt

cubeFromTerms :: [Term] -> Cube
cubeFromTerms ts = Cube {terms = ts, noOfTerms = length ts, minTerms = sort allTerms}
    where
        allTerms = map (minTermFromValues . map termToInt) (removeAllDC ts)

minTermFromValues :: [Int] -> Int
minTermFromValues = foldl (\acc x -> 2 * acc + x) 0

termToInt :: Term -> Int
termToInt One  = 1
termToInt Zero = 0
termToInt DC   = error "Cannot convert DC to Int"

removeFirstDC :: [Term] -> [[Term]]
removeFirstDC term = term1 : [term2]
    where
        term1 = front ++ One:trimmedBack
        term2 = front ++ Zero:trimmedBack
        (front, back) = break (==DC) term
        trimmedBack = tail back

removeAllDC :: [Term] -> [[Term]]
removeAllDC term = removeAllDCRec [term]
    where
        removeAllDCRec xs@(x:_)
            | DC `elem` x = removeAllDCRec $ concatMap removeFirstDC xs
            | otherwise   = xs

canJoin :: [Term] -> [Term] -> Bool
canJoin [] [] = True
canJoin [] _  = False
canJoin _ []  = False
canJoin (x:xs) (y:ys)
    | x == y             = canJoin xs ys
    | x == DC || y == DC = False
    | otherwise          = xs == ys

canJoinCube :: Cube -> Cube -> Bool
canJoinCube x y = canJoin (terms x) (terms y)
