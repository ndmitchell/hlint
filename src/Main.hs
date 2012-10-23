{-
HLint, Haskell source code suggestions
Copyright (C) 2006-2012, Neil Mitchell

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 2 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main where

import Language.Haskell.HLint
import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    errs <- hlint args
    when (length errs > 0) $
        exitWith $ ExitFailure 1

