#!/bin/bash

########################################
############# CSCI 2951-O ##############
########################################

if [[ $1 = 'haskell' ]]; then
    # compiling haskell source
    if [[ $(hostname) = 'NPx1carbon' ]]; then
        ghc -dynamic -o bin/solver -odir bin/ -hidir bin/ src/solver.hs
    else 
        ghc -o bin/solver -odir bin/ -hidir bin/ src/solver.hs
    fi
else
    # compiling python source
    python -m compileall src/solver.py -l -f
fi
