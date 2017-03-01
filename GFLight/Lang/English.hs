{-# LANGUAGE TypeFamilies #-}
module GFLight.Lang.English where

import Prelude hiding (Word)
import GFLight

-------------------------------------------------------------------------------------

class English a where
  type Lin a
  lin :: a -> Lin a

-------------------------------------------------------------------------------------

instance English (Word a) where
  type Lin (Word a) = String
  lin = linWord

instance English Str where
  type Lin Str = String
  lin = linStr

-------------------------------------------------------------------------------------
