#!/bin/bash
ghc Main.hs -O3 -j -o calculator -i. -no-keep-hi-files -no-keep-o-files
