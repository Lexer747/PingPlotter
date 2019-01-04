#!/bin/bash

echo "Setting up cabal sandbox"
cabal sandbox init 2>/dev/null >/dev/null
echo "Installing dependancies ..."
echo "terminal-size..."
cabal install terminal-size 2>/dev/null >/dev/null
echo "time..."
cabal install time 2>/dev/null >/dev/null
echo "Dependancies installed, building..."
cabal build 2>/dev/null >/dev/null
ln -s ./dist/build/Ping-v2-0-0/Ping-v2-0-0 ./Ping-v2-0-0
echo "Finished, use the Ping-v2-0-0 executable"
