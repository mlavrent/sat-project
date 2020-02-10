#!/bin/bash

########################################
############# CSCI 2951-O ##############
########################################

# Update this file with instructions on how to compile your code
ghc src/solver.hs

mkdir -p bin/
if test -f "src/solver"; then
	mv src/solver bin/
fi
if test -f "src/solver.exe"; then
	mv src/solver.exe bin/
fi
if test -f "src/solver.o"; then
	mv src/solver.o bin/
fi
if test -f "src/solver.hi"; then
	mv src/solver.hi bin/
fi

