#!/bin/bash
set -e

./scripts/test.sh
./scripts/hindent.sh
./scripts/hlint.sh

printf "Good work!\n"
