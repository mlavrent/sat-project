#!/bin/bash

########################################
############# CSCI 2951-O ##############
########################################

# Update this file with instructions on how to compile your code
if [[ $(hostname) = 'NPx1carbon' ]]; then
    ghc -dynamic -o bin/solver -odir bin/ -hidir bin/ src/solver.hs
else 
    ghc -o bin/solver -odir bin/ -hidir bin/ src/solver.hs
fi 
