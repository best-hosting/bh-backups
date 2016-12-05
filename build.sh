#!/bin/sh
mkdir -p _shake
ghc -isrc --make src/Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"
