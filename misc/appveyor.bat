setlocal
rem Put curl on the PATH
set PATH=C:\Program Files\Git\mingw64\bin;%PATH%
set VERSION=2.0.6
mkdir %TEMP%\hlint
curl -o%TEMP%\hlint\hlint.zip -L --insecure https://github.com/ndmitchell/hlint/releases/download/v%VERSION%/hlint-%VERSION%-x86_64-windows.zip
7z x %TEMP%\hlint\hlint.zip -o%TEMP%\hlint -y
%TEMP%\hlint\hlint-%VERSION%\hlint.exe %*
