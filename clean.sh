#!/bin/bash

rm Ping-v2-0-0
cabal clean
cabal sandbox delete
echo "Success"
