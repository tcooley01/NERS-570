#!/bin/bash

wget http://www-personal.umich.edu/~bkochuna/NERS570/Lab2/Ex2.tar.gz

tar -xf Ex2.tar.gz

cd ./Ex2

find . -maxdepth 1 -name nuclear_secrets1.txt -o -name nuclear_secrets2.txt

diff nuclear_secrets1.txt nuclear_secrets2.txt >> launch_code.txt


