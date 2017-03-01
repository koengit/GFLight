{-# LANGUAGE GADTs, DeriveDataTypeable, ScopedTypeVariables #-}
module GFLight
  ( P(..)
  , app
  , choice
  , hole
  , show'
  , (++), (+++), glue
  , parse
  , parseTrace
  , pMaybe
  , pList
  , pList2
  , pair
  , Str(..)
  , Word
  , wrd
  , linWord
  , get
  , dict
  )
 where

import Prelude hiding ((++),Word)
import qualified Prelude as P
import System.IO.Unsafe( unsafePerformIO )
import Control.Exception as E
import Data.Typeable
import Data.List hiding ((++))
import Data.Maybe

infixl 4 `App`

-------------------------------------------------------------------------------------

data P a where
  Unit   :: a -> P a
  App    :: P (a->b) -> P a -> P b
  Choice :: [P a] -> P a

app :: P (a->b) -> P a -> P b
Unit f `app` Unit x = Unit (f x)
pf     `app` px     = App pf px

choice :: [P a] -> P a
choice ps = ch [] ps
 where
  ch [q] []               = q
  ch qs  []               = Choice (reverse qs)
  ch qs  (Choice cs : ps) = ch qs (cs ++ ps)
  ch qs  (p:ps)           = ch (p:qs) ps

instance Functor P where
  fmap f p = Unit f `app` p

-------------------------------------------------------------------------------------

data Hole where
  Hole :: Int -> Hole
 deriving ( Typeable, Eq )

instance Show Hole where
  show (Hole k) = "?" ++ show k

instance Exception Hole

hole :: Int -> a
hole k = throw (Hole k)

peekHole :: a -> Maybe Int
peekHole x =
  unsafePerformIO $
    (do evaluate x
        return Nothing) `E.catch` \(Hole k) -> return (Just k)

isHole :: a -> Bool
isHole x = isJust (peekHole x)

-------------------------------------------------------------------------------------

lift :: [a] -> [a]
lift xs | Just h <- peekHole xs = [hole h]
lift (x:xs)                     = x : lift xs
lift []                         = []

show' :: String -> String
show' s = sh (lift s)
 where
  sh ""                           = ""
  sh (c:s) | Just h <- peekHole c = "?" ++ show h ++ sh s
  sh (c:s)                        = c : sh s

(++) :: [a] -> [a] -> [a]
xs ++ ys = lift xs P.++ ys

(+++) :: String -> String -> String
s1 +++ s2 = lift s1 +.+ lift s2
 where
  s1 +.+ s2
    | all isHole s1 || all isHole s2 = s1 P.++ s2
    | otherwise                      = s1 P.++ " " P.++ s2

glue :: [String] -> String
glue = foldr (+++) ""

{-
(=>=) :: String -> String -> Bool
xs =>= ys = match 0 0
 where
  ys' = lift ys
  n   = length xs + 1
  m   = length ys' + 1
  eof = "\EOT"

  mat = [ [ match' ix jy | jy <- [0..]`zip`(ys'++eof) ]
        | ix <- [0..]`zip`(xs++eof)
        ]

  match i j
    | i >= n    = j >= m
    | j >= m    = False
    | otherwise = (mat !! i) !! j

  match' (i,x) (j,y)
    -- | isHole y  = or [ match (i+k) (j+1) | k <- [0..n-i] ]
    | isHole y  = match (i+1) j || match i (j+1)
    | otherwise = x == y && match (i+1) (j+1)
-}

{-
dyn :: c -> (b -> c -> c) -> (a -> b -> c -> c -> c -> c) -> [a] -> [b] -> c
dyn h0 h1 h2 xs ys = head (mat xs ys)
 where
  mat []     ys = scanr h1 h0 ys
  mat (x:xs) ys = row x ys (mat xs ys)

  row x []     _                              = []
  row x (y:ys) ~(f_xs_yys:f_xs_s@(f_xs_ys:_)) =
    h2 x y f_xxs_ys f_xs_yys f_xs_ys : f_xxs_s
   where
    ~f_xxs_s@(f_xxs_ys:_) = row x ys f_xs_s

(=>=) :: String -> String -> Bool
xs =>= ys = dyn m_nil_nil m_nil_y m (xs++eof) (lift ys++eof)
 where
  eof = "\EOT"

  m_nil_nil     = True
  m_nil_y y b   = isHole y && b

  m x y m_xxs_ys m_xs_yys m_xs_ys
    | isHole y  = m_xxs_ys || m_xs_yys
    | otherwise = x == y && m_xs_ys
-}

(=>=) :: String -> String -> Bool
xs =>= ys = match xs (lift ys)
 where
  match xs []   = null xs
  match xs (y:ys)
    | isHole y  = any (`match` dropWhile isHole ys) (tails xs)
    | otherwise = not (null xs) && head xs == y && match (tail xs) ys

-------------------------------------------------------------------------------------

value :: P a -> Int -> (a, Int -> [P a], Int)
value (Unit x) n =
  (x, \_ -> [], n)

value (App pf px) n =
  let (f, hf, n1) = value pf n
      (x, hx, n2) = value px n1
   in (f x, \h -> if h < n1 then [ app qf px | qf <- hf h ]
                            else [ app pf qx | qx <- hx h ], n2)

value (Choice ps) n =
  let n' = n+1
   in n' `seq` (hole n, \_ -> ps, n')

solve :: P a -> (a -> Bool) -> [a]
solve p pr = search [p]
 where
  search []                = []
  search (p:ps)
    | Just h <- peekHole b = search (hf h ++ ps)
    | otherwise            = [ x | b ] ++ search ps
   where
    (x,hf,n) = value p 0
    b        = pr x

solveTrace :: P a -> (a -> Bool) -> ([a],Maybe a)
solveTrace p pr = search [p]
 where
  search [] =
    ([], Nothing)
  
  search (p:ps) =
    case peekHole b of
      Nothing
        | b         -> ([], Just x)
        | otherwise -> (x:ts, mx)
       where
        (ts,mx) = search ps
      
      Just h ->
        (x:ts, mx)
       where
        (ts, mx) = search (hf h ++ ps)
   where
    (x,hf,n) = value p 0
    b        = pr x

-------------------------------------------------------------------------------------

solve0 :: P a -> (a -> Bool) -> [a]
solve0 (Unit x)    pr = [ x | pr x ]
solve0 (App pf px) pr = [ f x | f <- solve0 pf (\f -> pr (f (hole 0))), x <- solve0 px (\x -> pr (f x)) ]
solve0 (Choice ps) pr = concat [ solve0 p pr | p <- ps ]

-------------------------------------------------------------------------------------

parse :: P a -> (a -> String) -> String -> [a]
parse p lin s = solve p (\x -> let s' = lin x in s =>= s' && s' == s')
--parse p lin s = solve0 p (\x -> let s' = lin x in s =>= s')

parseTrace :: P a -> (a -> String) -> String -> ([String],Maybe a)
parseTrace p lin s = (map (show' . lin) ts, mx)
 where
  (ts,mx) = solveTrace p (\x -> let s' = lin x in s =>= s' && s' == s')

-------------------------------------------------------------------------------------

pMaybe :: P a -> P (Maybe a)
pMaybe p = choice [ Unit Nothing, Unit Just `App` p ]

pList :: P a -> P [a]
pList p = choice [ Unit [], Unit (:) `App` p `App` pList p ]

pList2 :: P a -> P [a]
pList2 p = choice [ Unit [], Unit (:) `App` p `App` Choice [ Unit [], Unit (:[]) `App` p ] ]

pair :: P a -> P b -> P (a,b)
pair p q = Unit (,) `App` p `App` q

-------------------------------------------------------------------------------------

newtype Str = Str{ linStr :: String }

instance Show Str where
  show = linStr

-------------------------------------------------------------------------------------

data Word a
  = Nil a
  | Letter Char (Word a)

instance Show a => Show (Word a) where
  show w = show (linWord w) ++ ":" ++ show (get w)

wrd :: a -> String -> Word a
wrd x ""    = Nil x
wrd x (c:s) = Letter c (wrd x s)

get :: Word a -> a
get (Nil x)      = x
get (Letter _ w) = get w

linWord :: Word a -> String
linWord (Nil _)      = ""
linWord (Letter c w) = c : linWord w

dict :: [(a,String)] -> P (Word a)
dict ws = choice ( [ Unit (Nil x) | (x,"") <- ws ]
                ++ [ Unit (Letter c) `App` dict [ (x,s) | (x,c':s) <- ws, c == c' ]
                   | c <- cs
                   ]
                 )
 where
  cs = map head (group (sort [ c | (_,c:_) <- ws ]))

-------------------------------------------------------------------------------------
