#!/bin/bash

EXE=GenerateJSONParser
SRC=${EXE}.hs

# TODO: add ParseJSON.hs
ghc --make ${SRC} -o ${EXE} && \
for i in test/*.json examples/*.json; do
  basename $i
  echo ./${EXE} $i
  OUT=`basename $i .json`.hs
  time ./${EXE} $i --outputFilename ${OUT} && ghc ${OUT} || exit 1
  runghc ${OUT} ${i} || exit 2
done
echo Finished
