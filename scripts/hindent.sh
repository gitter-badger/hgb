#!/bin/bash
set -e

printf "Running hindent\n"
hindent --validate src/**.hs test/*/**.hs # for testing via travis
printf "Hindent succesfull\n\n"
