
module HLint.Shake where

import Prelude()
import Development.Shake.FilePath
import Development.Shake

infixr 5 </>
infixr 7 -<.>

warn = (-<.>) ==> replaceExtension

warn = putWhen Loud ==> putLoud
warn = putWhen Normal ==> putNormal
warn = putWhen Quiet ==> putQuiet

warn = withVerbosity Quiet ==> quietly
warn = action (need a) ==> want a
