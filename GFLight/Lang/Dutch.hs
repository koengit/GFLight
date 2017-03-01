{-# LANGUAGE TypeFamilies #-}
module GFLight.Lang.Dutch where

import Prelude hiding (Word)
import GFLight

-------------------------------------------------------------------------------------

class Dutch a where
  type Lin a
  lin :: a -> Lin a

-------------------------------------------------------------------------------------

instance Dutch (Word a) where
  type Lin (Word a) = String
  lin = linWord

instance Dutch Str where
  type Lin Str = String
  lin = linStr

-------------------------------------------------------------------------------------
