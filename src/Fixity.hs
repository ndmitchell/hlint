
module Fixity(
    FixityInfo, Associativity(..),
    toHseFixity, fromHseFixity,
    fromFixitySig, toFixitySig, toFixity, toHseFixities,
    preludeFixities, baseFixities, -- From GhclibParserEx
    lensFixities, otherFixities, customFixities
    ) where

import GHC.Generics(Associativity(..))
import qualified Language.Haskell.Exts as HSE (Fixity(..), QName(..), Name(..), Assoc(..), SpecialCon(..))
import HsBinds
import HsExtension
import OccName
import RdrName
import SrcLoc
import BasicTypes
import Language.Haskell.GhclibParserEx.Fixity

-- Lots of things define a fixity. None define it quite right, so let's have our own type.

-- | A Fixity definition, comprising the name the fixity applies to,
--   the direction and the precedence. As an example:
--
-- > infixr 3 `foo`
--
--   Would create @(\"foo\", RightAssociative, 3)
type FixityInfo = (String, Associativity, Int)

fromHseFixity :: HSE.Fixity -> FixityInfo
fromHseFixity (HSE.Fixity dir i name) = (g name, f dir, i)
    where
        f HSE.AssocLeft{} = LeftAssociative
        f HSE.AssocRight{} = RightAssociative
        f HSE.AssocNone{} = NotAssociative

        g :: HSE.QName () -> String
        g (HSE.Special _ HSE.Cons{}) = ":"
        g (HSE.Special _ HSE.UnitCon{}) = "()"
        g (HSE.UnQual _ (HSE.Ident _ x)) = x
        g (HSE.UnQual _ (HSE.Symbol _ x)) = x
        g _ = ""

toHseFixity :: FixityInfo -> HSE.Fixity
toHseFixity (name, dir, i) = HSE.Fixity (f dir) i $ HSE.UnQual () $ HSE.Ident () name
    where
        f LeftAssociative = HSE.AssocLeft ()
        f RightAssociative = HSE.AssocRight ()
        f NotAssociative = HSE.AssocNone ()

fromFixitySig :: FixitySig GhcPs -> [FixityInfo]
fromFixitySig (FixitySig _ names (Fixity _ i dir)) =
    [(occNameString $ occName $ unLoc name, f dir, i) | name <- names]
    where
        f InfixL = LeftAssociative
        f InfixR = RightAssociative
        f InfixN = NotAssociative
fromFixitySig _ = []

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
toFixitySig = mkFixitySig . toFixity
  where
    mkFixitySig :: (String, Fixity) -> FixitySig GhcPs
    mkFixitySig (name, x) = FixitySig noExt [noLoc $ mkRdrUnqual (mkVarOcc name)] x

toHseFixities :: [(String, Fixity)] -> [HSE.Fixity]
toHseFixities = map (toHseFixity  . fromFixity)

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
