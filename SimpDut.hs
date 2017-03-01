{-# LANGUAGE TypeFamilies #-}
module SimpDut where

import GFLight
import Prelude hiding ((++))
import GFLight.Lang.Dutch

import SimpAbs

-------------------------------------------------------------------------------------
-- dutch

-- linearizations

data Gender = Neutrum | Utrum deriving ( Show )

instance Dutch S where
  type Lin S = String
  lin (S np vp) = s +++ lin vp nb
   where
    (s,nb) = lin np

instance Dutch NP where
  type Lin NP = (String, Numb)
  lin (NP nb art adjs n) = (lin art g nb +++ glue [ lin a art g nb | a <- adjs ] +++ f nb, nb)
   where
    (f,g) = lin n
  lin (With np1 np2) = (s1 +++ "met" +++ s2, n1)
   where
    (s1,n1) = lin np1
    (s2,n2) = lin np2

instance Dutch VP where
  type Lin VP = Numb -> String
  lin (VP v mnp) nb = lin v nb +++ case mnp of
                                     Nothing -> ""
                                     Just np -> fst (lin np)

instance Dutch Noun where
  type Lin Noun = (Numb -> String, Gender)
  lin Telescope = (\n -> "telesco" ++ case n of
                                        Sg -> "op"
                                        Pl -> "pen", Utrum)
  lin Girl      = (\n -> "meisje" ++ case n of
                                       Sg -> ""
                                       Pl -> "s", Neutrum)

instance Dutch Verb where
  type Lin Verb = Numb -> String
  lin See  = \n -> "zie" ++ case n of
                              Sg -> "t"
                              Pl -> "n"
  lin Love = \n -> "bemin" ++ case n of
                                Sg -> "t"
                                Pl -> "nen"

instance Dutch Adj where
  type Lin Adj = Art -> Gender -> Numb -> String
  lin Beautiful = \art g n -> "mooi" ++ case (art, g,n) of
                                          (Indet, Neutrum, Sg) -> ""
                                          _                    -> "e"
  lin Red       = \art g n -> "ro" ++ case (art, g,n) of
                                        (Indet, Neutrum, Sg) -> "od"
                                        _                    -> "de"

instance Dutch Art where
  type Lin Art = Gender -> Numb -> String
  lin Det Neutrum Sg = "het"
  lin Det _       _  = "de"
  lin Indet _     Sg = "een"
  lin Indet _     Pl = ""

-------------------------------------------------------------------------------------
