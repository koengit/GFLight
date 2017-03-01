{-# LANGUAGE TypeFamilies #-}
module Main where

import GFLight
import Prelude hiding ((++))

import SimpAbs
import SimpEng as E
import GFLight.Lang.English as E
import SimpDut as D
import GFLight.Lang.Dutch as D

import System.IO

-------------------------------------------------------------------------------------

main0 =
  do s <- getLine
     case parse E.lang E.lin s of
       []  -> putStrLn "(no parse)"
       x:_ -> do putStrLn "parsed!"
                 putStrLn ("Dutch: " ++ show' (D.lin x))
     main

main1 = putStr $ unlines $ map (show' . D.lin) $ take 5 $ parse E.lang E.lin s1

main =
  do putStr $ unlines $ ts
     case mx of
       Nothing -> putStrLn "No parse!"
       Just x  -> putStrLn ("==> " ++ show' (D.lin x))
 where
  (ts,mx) = parseTrace E.lang E.lin s2

s1 = "the beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy beautiful lazy beautiful lazy beautiful lazy lazy beautiful lazy girl sees the beautiful lazy beautiful lazy beautiful lazy girl"
s2 = "the beautiful fun ugly lazy smart rich beautiful fun ugly lazy smart rich man with a girl sees a beautiful fun ugly lazy smart rich beautiful fun ugly lazy smart rich car"
s3 = "het mooie luie leuke meisje ziet de lelijke slimme vrouw"
s4 = "the beautiful girl with a ugly man sees the girl with the girl with the man"

-------------------------------------------------------------------------------------
