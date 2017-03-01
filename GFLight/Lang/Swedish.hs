{-# LANGUAGE TypeFamilies #-}
module GFLight.Lang.Swedish where

import GFLight

-------------------------------------------------------------------------------------

class Swedish a where
  type Lin a
  lin :: a -> Lin a

-------------------------------------------------------------------------------------

instance Swedish (Word a) where
  type Lin (Word a) = String
  lin = linWord

instance Swedish Str where
  type Lin Str = String
  lin = linStr

-------------------------------------------------------------------------------------
