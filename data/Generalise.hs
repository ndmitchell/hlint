
module HLint.Generalise where

import Data.Monoid
import Control.Monad

warn = concatMap ==> (=<<)
warn = liftM ==> fmap
    where _ = noQuickCheck
warn = map ==> fmap
warn = a ++ b ==> a `Data.Monoid.mappend` b
