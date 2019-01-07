@echo off
echo Setting up cabal sandbox
cabal sandbox init 2>nul >nul
echo Installing dependancies ...
echo terminal-size...
cabal install terminal-size 2>nul >nul
echo time...
cabal install time 2>nul >nul
echo regex-posix...
cabal install regex-posix 2>nul >nul
echo Dependancies installed, building...
cabal build 2>nul >nul
ln -s .\dist\build\Ping-v2-0-0\Ping-v2-0-0.exe .\Ping-v2-0-0.exe
echo Finished, use the Ping-v2-0-0 executable