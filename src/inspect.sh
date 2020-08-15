#!/bin/zsh
set -e

echo "Program:"
cat $PROGRAM
echo "\nParser:"
make > /dev/null
freak -p $PROGRAM
echo "\nCPS:"
freak -c $PROGRAM
echo "----------------------"
echo "\nEval:"
freak $PROGRAM
