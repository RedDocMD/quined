module Lib
    ( Term (One, Zero, DC)
    , Cube (Cube)
    , terms
    , noOfTerms
    , minTerms
    , unitCube
    ) where

import           Data.Bits

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

unitCube :: Int -> Int -> Maybe Cube
unitCube mt cnt
    | length thisTerms == cnt = Just Cube {noOfTerms = cnt
                                          ,terms = thisTerms
                                          ,minTerms = [mt]}
    | otherwise = Nothing
    where
        thisTerms = findTermsPadded mt cnt
