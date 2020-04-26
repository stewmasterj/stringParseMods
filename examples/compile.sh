#!/bin/bash
if [ -z $1 ]; then
 echo "ERROR; must specify a file to compile"
else
  `grep "compile:" ${1} | cut -d ":" -f 2`
fi
