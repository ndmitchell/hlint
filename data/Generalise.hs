
module HLint.Generalise where

import Data.Monoid
import Control.Monad

hint = concatMap ==> (=<<)
hint = liftM ==> fmap
    where _ = noQuickCheck
hint = map ==> fmap
hint = a ++ b ==> a `Data.Monoid.mappend` b
