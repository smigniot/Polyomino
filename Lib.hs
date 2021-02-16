module Lib
    ( someFunc
    , buildShapes
    , buildSortedShapes
    , buildSolutions
    , showShapes
    ) where

import Data.List (sort, sortOn, nub, intercalate, intersect)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Shape = [(Int,Int)]

buildShapes :: Int -> [Shape]
buildShapes 1 = [ [(0,0)] ]
buildShapes n = nextShapes (buildShapes (n-1))

buildSortedShapes :: Int -> [Shape]
buildSortedShapes n =
    let variantCount shape = length (variantsof shape)
        in sortOn variantCount (buildShapes n)

nextShapes :: [Shape] -> [Shape]
nextShapes shapes =
    let augmented = nub (foldr (++) [] (map augment shapes))
        referenceof s = head (sort (variantsof s))
        in nub (map referenceof augmented)

augment :: Shape -> [Shape]
augment shape = 
    let shifted = map (\(x,y) -> (x+1,y+1)) shape
        candidates = foldr (++) [] (map tldr shifted)
        tldr (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        remaining = [p | p<-(nub candidates), not (elem p shifted)]
        generated = [ (p:shifted) | p <- remaining ]
        in map cornered generated

cornered :: Shape -> Shape
cornered shape =
    let lx = map fst shape
        x0 = foldr min (head lx) lx
        ly = map snd shape
        y0 = foldr min (head ly) ly
        f (x,y) = (x-x0,y-y0)
        in map f shape

variantsof :: Shape -> [Shape]
variantsof shape =
    let lx = map fst shape
        xm = foldr max (head lx) lx
        ly = map snd shape
        ym = foldr max (head ly) ly
        s2 = map (\(x,y) -> (xm-x,y)) shape
        s3 = map (\(x,y) -> (x,ym-y)) shape
        s4 = map (\(x,y) -> (xm-x,ym-y)) shape
        s5 = map (\(x,y) -> (ym-y,x)) shape
        s6 = map (\(x,y) -> (ym-x,y)) s5
        s7 = map (\(x,y) -> (x,xm-y)) s5
        s8 = map (\(x,y) -> (ym-x,xm-y)) s5
        flavors = map sort [shape,s2,s3,s4,s5,s6,s7,s8]
        in nub flavors

showShapes :: [Shape] -> String
showShapes shapes =
    let shift [] _ = []
        shift (s:ss) x0 =
            let lx2 = map fst s
                xm2 = foldr max (head lx2) lx2
                s' = map (\(x,y)->(x+x0,y)) s
                in (s':(shift ss (x0+xm2+2)))
        shifted = shift shapes 0
        flat = foldr (++) [] shifted
        lx = map fst flat
        xm = foldr max (head lx) lx
        ly = map snd flat
        ym = foldr max (head ly) ly
        aschar True = 'X'
        aschar _ = ' '
        line y = map aschar [elem (x,y) flat | x <- [0..xm]]
        in intercalate "\n" [line y | y <- [0..ym]]

type State = Shape
type Solution = Shape

buildSolutions :: Int -> Int -> Int -> [Solution]
buildSolutions n w h =
    let shapes = buildSortedShapes n
        in allSolutions n w h shapes []

allSolutions :: Int -> Int -> Int -> [Shape] -> State -> [Solution]
allSolutions n w h [] state = [state]
allSolutions n w h (shape:others) state =
    let variants = variantsof shape
        allvariants = foldr (++) [] (map (allPositions w h) variants)
        possible = filter (isCompatible n w h state) allvariants
        recurse l = allSolutions n w h others (l++state)
        in foldr (++) [] (map recurse possible)

isCompatible :: Int -> Int -> Int -> State -> Shape -> Bool
isCompatible n w h a b = (null (intersect a b))
    && (holesMultiples n w h (a++b))

holesMultiples :: Int -> Int -> Int -> State -> Bool
holesMultiples n w h state =
    let coordinates = [(x,y) | y<-[0..(h-1)], x<-[0..(w-1)]]
        used = S.fromList state
        empties = [p | p<-coordinates, not (S.member p used)]
        holes = countHoles empties
        in null (filter (\h->((rem h n)/=0)) holes)

countHoles :: State -> [Int]
countHoles state = --trace ("DBG: "++(show state)) 
    (countHoles' state M.empty M.empty S.empty)
countHoles' [] labels counts equivalents =
    reconcile counts (S.toList equivalents)
countHoles' ((x,y):ps) labels counts equiv =
    let north = M.lookup (x,y-1) labels
        west =  M.lookup (x-1,y) labels
        label = labelof north west counts
        newlabels = M.insert (x,y) label labels
        existing = case M.lookup label counts of
            Nothing -> 0
            Just n -> n
        newcounts = M.insert label (existing+1) counts
        in countHoles' ps newlabels newcounts (recordequiv north west equiv)
labelof Nothing Nothing count = 1 + (foldr (max) 0 (M.keysSet count))
labelof Nothing (Just label) _ = label
labelof (Just label) Nothing _ = label
labelof (Just l1) (Just l2) _ = min l1 l2
recordequiv :: Maybe Int -> Maybe Int -> S.Set (Int,Int) -> S.Set (Int,Int)
recordequiv (Just l1) (Just l2) e 
    | l1 /= l2 = S.insert ((min l1 l2),(max l1 l2)) e
    | otherwise = e
recordequiv _ _ e = e
reconcile :: M.Map Int Int -> [(Int,Int)] -> [Int]
reconcile counts equivalents = 
    let initial = M.fromList [(o,o) | o <- (M.keys counts)]
        tree = inserted equivalents initial
        inserted [] t = t
        inserted ((a,b):l) t = inserted l (union a b t)
        union a b t = M.insert (find a t) (find b t) t
        find a t = case M.lookup a t of
            Nothing -> a
            Just c -> if a == c
                         then a
                         else find c t
        byparent = map (\(a,c)->(find a tree,c)) (M.toList counts)
        cmap = reduce' byparent M.empty
        reduce' [] m = m
        reduce' ((p,n):l) m =
            let m2 = case M.lookup p m of
                        Nothing -> M.insert p n m
                        Just s -> M.insert  p (n+s) m
                in reduce' l m2
        in map snd (M.toList cmap)
    
allPositions :: Int -> Int -> Shape -> [State]
allPositions w h shape =
    let lx = map fst shape
        xm = foldr max (head lx) lx
        ly = map snd shape
        ym = foldr max (head ly) ly
        placeat x0 y0 = map (\(x,y)->(x+x0,y+y0)) shape
        in [(placeat x y) | x<-[0..(w-xm-1)], y<-[0..(h-ym-1)]]


