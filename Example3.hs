{-# LANGUAGE TypeFamilies #-}
module Main where

import GFLight
import Prelude hiding ((++),Word)

import SimpAbs
import SimpEng as E
import GFLight.Lang.English as E
import SimpDut as D
import GFLight.Lang.Dutch as D

-------------------------------------------------------------------------------------

main =
  do s <- getLine
{-
     case parse pS E.lin s of
       []  -> putStrLn "(no parse)"
       x:_ -> do putStrLn "parsed!"
                 putStrLn (show' (D.lin x))
-}
     case parseTrace pS E.lin s of
       (_, Nothing) ->
         do putStrLn "(no parse)"
       
       (tr, Just x) ->
         do putStrLn "parsed!"
            putStr (unlines tr)
            putStrLn (show' (D.lin x))
     main

s = "the beautiful red telescope loves the beautiful red red girl"

-------------------------------------------------------------------------------------
