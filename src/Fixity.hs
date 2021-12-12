{-# LANGUAGE ViewPatterns #-}

module Fixity(
    FixityInfo, Associativity(..),
    defaultFixities,
    fromFixitySig, toFixitySig, toFixity,
    ) where

import GHC.Generics(Associativity(..))
import GHC.Hs.Binds
import GHC.Hs.Extension
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Parser.Annotation
import Language.Haskell.Syntax.Extension
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import Language.Haskell.GhclibParserEx.Fixity

-- Lots of things define a fixity. None define it quite right, so let's have our own type.

-- | A Fixity definition, comprising the name the fixity applies to,
--   the direction and the precedence. As an example, a source file containing:
--
-- > infixr 3 `foo`
--
--   would create @(\"foo\", RightAssociative, 3)@.
type FixityInfo = (String, Associativity, Int)

fromFixitySig :: FixitySig GhcPs -> [FixityInfo]
fromFixitySig (FixitySig _ names (Fixity _ i dir)) =
    [(rdrNameStr name, f dir, i) | name <- names]
    where
        f InfixL = LeftAssociative
        f InfixR = RightAssociative
        f InfixN = NotAssociative

toFixity :: FixityInfo -> (String, Fixity)
toFixity (name, dir, i) = (name, Fixity NoSourceText i $ f dir)
    where
        f LeftAssociative = InfixL
        f RightAssociative = InfixR
        f NotAssociative = InfixN

fromFixity :: (String, Fixity) -> FixityInfo
fromFixity (name, Fixity _ i dir) = (name, assoc dir, i)
  where
    assoc dir = case dir of
      InfixL -> LeftAssociative
      InfixR -> RightAssociative
      InfixN -> NotAssociative

toFixitySig :: FixityInfo -> FixitySig GhcPs
toFixitySig (toFixity -> (name, x)) = FixitySig noExtField [noLocA $ mkRdrUnqual (mkVarOcc name)] x

defaultFixities :: [FixityInfo]
defaultFixities = map fromFixity $ customFixities ++ baseFixities ++ lensFixities ++ otherFixities

-- List as provided at https://github.com/ndmitchell/hlint/issues/416.
lensFixities :: [(String, Fixity)]
lensFixities = concat
    [ infixr_ 4 ["%%@~","<%@~","%%~","<+~","<*~","<-~","<//~","<^~","<^^~","<**~"]
    , infix_ 4 ["%%@=","<%@=","%%=","<+=","<*=","<-=","<//=","<^=","<^^=","<**="]
    , infixr_ 2 ["<<~"]
    , infixr_ 9 ["#."]
    , infixl_ 8 [".#"]
    , infixr_ 8 ["^!","^@!"]
    , infixl_ 1 ["&","<&>","??"]
    , infixl_ 8 ["^.","^@."]
    , infixr_ 9 ["<.>","<.",".>"]
    , infixr_ 4 ["%@~",".~","+~","*~","-~","//~","^~","^^~","**~","&&~","<>~","||~","%~"]
    , infix_ 4 ["%@=",".=","+=","*=","-=","//=","^=","^^=","**=","&&=","<>=","||=","%="]
    , infixr_ 2 ["<~"]
    , infixr_ 2 ["`zoom`","`magnify`"]
    , infixl_ 8 ["^..","^?","^?!","^@..","^@?","^@?!"]
    , infixl_ 8 ["^#"]
    , infixr_ 4 ["<#~","#~","#%~","<#%~","#%%~"]
    , infix_ 4 ["<#=","#=","#%=","<#%=","#%%="]
    , infixl_ 9 [":>"]
    , infixr_ 4 ["</>~","<</>~","<.>~","<<.>~"]
    , infix_ 4 ["</>=","<</>=","<.>=","<<.>="]
    , infixr_ 4 [".|.~",".&.~","<.|.~","<.&.~"]
    , infix_ 4 [".|.=",".&.=","<.|.=","<.&.="]
    ]

otherFixities :: [(String, Fixity)]
otherFixities = concat
  -- hspec
  [ infix_ 1 ["shouldBe","shouldSatisfy","shouldStartWith","shouldEndWith","shouldContain","shouldMatchList"
              ,"shouldReturn","shouldNotBe","shouldNotSatisfy","shouldNotContain","shouldNotReturn","shouldThrow"]
    -- quickcheck
  , infixr_ 0 ["==>"]
  , infix_ 4 ["==="]
    -- esqueleto
  , infix_ 4 ["==."]
    -- lattices
  , infixr_ 5 ["\\/"] -- \/
  , infixr_ 6 ["/\\"] -- /\
  ]

customFixities :: [(String, Fixity)]
customFixities =
  infixl_ 1 ["`on`"]
        -- See https://github.com/ndmitchell/hlint/issues/425
        -- otherwise GTK apps using `on` at a different fixity have
        -- spurious warnings.
