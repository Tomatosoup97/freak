#!/bin/zsh
set -e

if [ -z "$PROGRAM" ]
then
  echo "error: please set PROGRAM env variable with path to code"
  exit 1
fi


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
