Notes on how to develop HLint

TO DEBUG
========

Type ghci.bat in the root folder, it will open HLint in GHC.
Create Example.hs and put the sample code in there.
At GHCi type ":main Example.hs"


TO TEST
=======

Open a debugging session and type ":main --test", you should see:

*Main> :main --test
Tests passed (82)
