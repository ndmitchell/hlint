mkdir obj 2> nul
ghc --make -i%YHC_BASE_PATH%\..\src\libraries\core -hidir obj -odir obj -o drhaskell.exe Main 
