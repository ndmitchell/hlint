setlocal
rem Put curl on the PATH
set PATH=C:\Program Files\Git\mingw64\bin;%PATH%
set VERSION=2.0.5
mkdir %TEMP%\hlint
curl -o%TEMP%\hlint\hlint-%VERSION%.zip -L --insecure https://github.com/ndmitchell/hlint/releases/download/v%VERSION%/hlint-%VERSION%.zip
7z x %TEMP%\hlint\hlint-%VERSION%.zip -o%TEMP%\hlint -y
%TEMP%\hlint\hlint-%VERSION%\hlint.exe %*
