{-# LANGUAGE TypeFamilies #-}
module Main where

import GFLight
import Prelude hiding ((++))

import SimpAbs
import SimpEng as E
import GFLight.Lang.English as E

-------------------------------------------------------------------------------------

main =
  do s <- getLine
     case parse pS E.lin s of
       []  -> putStrLn "(no parse)"
       x:_ -> do putStrLn "parsed!"
                 putStrLn (show x)
     main

s = "the beautiful red telescope loves the beautiful red red girl"

-------------------------------------------------------------------------------------
