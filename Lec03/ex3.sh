#!/bin/bash

echo "Arg 0 is $0"
echo "Arg 1 is $1"
echo "Arg 2 is $2"
echo "the whole arg list is $@"

echo "The number of arguments is $#"
echo "The script name is $0"
echo "but we can trim file extensions ${0%.*}"
echo "We can also print sunstrings with specific indexes ${0:2}"
echo "or we can select substrings: ${0:2:3}"

for s in $@; do
    echo $s
done


