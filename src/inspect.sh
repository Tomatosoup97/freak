#!/bin/zsh
set -e

if [ -z "$PROGRAM" ]
then
  echo "error: please set PROGRAM env variable with path to code"
  exit 1
fi


echo "Compiling..."
make > /dev/null
echo "Program:"
cat $PROGRAM
echo "\nParser:"
freak -p $PROGRAM
echo "\nCPS:"
freak -c $PROGRAM
echo "----------------------"
echo "\nEval:"
freak $PROGRAM
