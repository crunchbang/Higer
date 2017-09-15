#!/bin/bash
set -euo pipefail
IFS=$'\n\t'


#read filepath
filepath=$1
echo $filepath
echo "Build stack"
echo "stack build"
stack build

echo "Running the parser"
echo "echo $filepath|stack exec Higer-exe"
echo $filepath|stack exec Higer-exe > /tmp/parse_out.txt

echo "Parse output"
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

echo "Compiling latex document"
latex -output-directory=/tmp /tmp/output.tex 2>&1 > /dev/null
#cleanup
echo "Cleanup"
rm /tmp/output.tex
rm /tmp/parse_out.txt
#open file
open /tmp/output.dvi
