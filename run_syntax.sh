#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
 
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

#read filepath
filepath=$1
echo $filepath
echo "${green}[HIGER]: Input program${reset}"
cat $filepath
echo ""
echo "${green}[HIGER]: Build files${reset}"
echo "stack build"
stack build

echo ""
echo "${green}[HIGER]: Running the parser${reset}"
echo "echo $filepath|stack exec Higer-exe"
echo $filepath|stack exec Higer-exe > /tmp/parse_out.txt

echo ""
echo "${green}[HIGER]: Syntax output${reset}"
cat /tmp/parse_out.txt
