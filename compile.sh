#!/bin/sh

ghc -O2 --make $1 -prof -auto-all -caf-all -fforce-recomp -rtsopts
