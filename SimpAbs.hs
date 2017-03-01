module SimpAbs where

import GFLight
import Prelude hiding ((++))

-------------------------------------------------------------------------------------
-- abstract syntax

-- structure

data S
  = S NP VP
 deriving ( Show )

pS = Unit S `App` pNP `App` pVP

data Numb = Pl | Sg 
 deriving ( Show )

pNumb = choice [ Unit Pl, Unit Sg ]

data NP
  = NP Numb Art [Adj] Noun
  | NP `With` NP
 deriving ( Show )

pNP = choice [ Unit NP `App` pNumb `App` pArt `App` pList pAdj `App` pNoun
             , Unit With `App` pNP `App` pNP
             ]

data VP
  = VP Verb (Maybe NP)
 deriving ( Show )

pVP = Unit VP `App` pVerb `App` pMaybe pNP

-- word categories

data Art = Det | Indet
 deriving ( Show )

pArt = choice [ Unit Det, Unit Indet ]

data Noun = Girl | Telescope
 deriving ( Show )
 
pNoun = choice [ Unit Girl, Unit Telescope ]

data Verb = See | Love
 deriving ( Show )

pVerb = choice [ Unit See, Unit Love ]

data Adj = Beautiful | Red
 deriving ( Show )

pAdj = choice [ Unit Beautiful, Unit Red ]

-------------------------------------------------------------------------------------


















{-
data Noun = Noun (Word String)
 deriving ( Show )

pNoun = Noun `fmap` dict [ (s,s) | s <- words "girl telescope television house horse" ]
-}

