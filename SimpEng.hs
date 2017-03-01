{-# LANGUAGE TypeFamilies #-}
module SimpEng where

import GFLight
import Prelude hiding ((++))
import GFLight.Lang.English

import SimpAbs

-------------------------------------------------------------------------------------
-- english

-- linearizations

instance English S where
  type Lin S = String
  lin (S np vp) = s +++ lin vp nb
   where
    (s,nb) = lin np

instance English NP where
  type Lin NP = (String, Numb)
  lin (NP nb art adjs n) = (lin art nb +++ glue [ lin a | a <- adjs ] +++ lin n nb, nb)
  lin (With np1 np2)     = let (s1,n1) = lin np1
                               (s2,n2) = lin np2
                            in (s1 +++ "with" +++ s2, n1)

instance English VP where
  type Lin VP = Numb -> String
  lin (VP v mnp) nb = lin v nb +++ case mnp of
                                     Nothing -> ""
                                     Just np -> fst (lin np)

instance English Art where
  type Lin Art = Numb -> String
  lin Det   _  = "the"
  lin Indet Sg = "a"
  lin Indet Pl = ""

instance English Noun where
  type Lin Noun = Numb -> String
  lin n nb = (case n of
                Telescope -> "telescope"
                Girl      -> "girl") ++ case nb of
                                          Sg -> ""
                                          Pl -> "s"

instance English Verb where
  type Lin Verb = Numb -> String
  lin v nb = (case v of
                See  -> "see"
                Love -> "love") ++ case nb of
                                     Sg -> "s"
                                     Pl -> ""

instance English Adj where
  type Lin Adj = String
  lin Beautiful = "beautiful"
  lin Red       = "red"

-------------------------------------------------------------------------------------












{-
instance English Noun where
  type Lin Noun = Numb -> String
  lin (Noun w) nb = linWord w ++ case nb of
                                   Sg -> ""
                                   Pl -> "s"
-}
