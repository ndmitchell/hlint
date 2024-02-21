-- |
haddockAboveSingle = 1

haddockBelowSingle = 1
-- ^

-- |
--
haddockAboveSingles = 1

-- |
-- >>>
haddockAboveDoctest = 1

-- |
--
--
-- foo
haddockAboveIntoNonEmpty = 1

haddockBelowIntoNonEmpty = 1
-- ^
--
--
-- foo

{--}
commentMultiEmpty = 1

{- -}
commentMultiEmptySpace = 1

{-

    -}
commentMultiEmptyLines = 1

{- |-}
haddockAboveMultiEmpty = 1

{- | -}
haddockAboveMultiEmptySpace = 1

{- |

    -}
haddockAboveMultiEmptyLines = 1

haddockBelowMultiEmpty = 1
{- ^-}

haddockBelowMultiEmptySpace = 1
{- ^-}

haddockBelowMultiEmptyLines = 1
{- ^

    -}

data HaddockSingle = HaddockSingle
  { haddockFieldEmptyBelow :: Int
  -- ^
  -- |
  , haddockFieldAboveEmpty :: Int
  , haddockFieldEmptyAfter :: Int -- ^
  }

data HaddockMulti = HaddockMulti
  { haddockFieldEmptyBelow :: Int
  {- ^-}
  {- |-}
  , haddockFieldAboveEmpty :: Int
  , haddockFieldEmptyAfter :: Int {- ^-}
  }