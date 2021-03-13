module Lib where

import           Data.Bits
import           Data.List
import           Data.List.Utils
import qualified Data.Map.Strict as M

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

-- Assume that the cubes passed can actually be joined
joinCube :: Cube -> Cube -> Cube
joinCube cube1 cube2 = cubeFromTerms $ joinTerms (terms cube1) (terms cube2)
    where
        flattenTermPair term1 term2 = if term1 == term2 then term1 else DC
        joinTerms = zipWith flattenTermPair

oneCount :: Cube -> Int
oneCount cube = countElem One (terms cube)

partitionCubes :: [Cube] -> M.Map Int [Cube]
partitionCubes = foldl (\m x -> mapAppend (oneCount x) x m) M.empty

mapAppend :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
mapAppend k v m =
    case M.lookup k m of
        Just list -> M.insert k (v:list) m
        Nothing   -> M.insert k [v] m

-- Joins all possible cubes for a given one-count
joinAdjacent :: M.Map Int [Cube] -> Int -> [Cube]
joinAdjacent m oc =
    case (M.lookup oc m, M.lookup (oc + 1) m) of
        (Just list1, Just list2) -> map (uncurry joinCube) $ filter (uncurry canJoinCube) [(x, y) | x <- list1, y <- list2]
        _                        -> []

joinAllAdjacent :: M.Map Int [Cube] -> Int -> [Cube]
joinAllAdjacent m mtc = foldl (\acc x -> acc ++ joinAdjacent m x) [] ocs
    where
        ocs = take mtc (iterate (+1) 0)
