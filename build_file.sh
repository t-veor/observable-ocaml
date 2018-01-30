#!/usr/bin/env bash

make

./main.native $1 -o ${1%.*}
gcc "${1%.*}.c" -Iruntime -o ${1%.*}
