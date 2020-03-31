#!/bin/bash
set -e 

printf "Running hlint\n"
hlint src/**.hs test/**/*.hs
printf "\n"
