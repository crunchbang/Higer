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
echo "${green}[HIGER]: Parse output${reset}"
cat /tmp/parse_out.txt

#create the tex file
echo "
\documentclass[png,border=4pt]{standalone}
\usepackage{forest}
\bracketset{
opening bracket=(,
  closing bracket=)}
\begin{document}
\begin{forest}" > /tmp/output.tex

cat /tmp/parse_out.txt >> /tmp/output.tex

echo " \end{forest} 
\end{document}" >> /tmp/output.tex

echo ""
echo "${green}[HIGER]: Compiling latex document${reset}"
latex -output-directory=/tmp /tmp/output.tex 2>&1 > /dev/null
#cleanup
echo ""
echo "${green}[HIGER]: Cleanup${reset}"
rm /tmp/output.tex
rm /tmp/parse_out.txt
#open file
echo ""
echo "${green}[HIGER]: Opening file${reset}"
open /tmp/output.dvi
