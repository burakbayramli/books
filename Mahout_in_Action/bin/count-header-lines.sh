#!/bin/bash
export LC_ALL='C'
for file in 20news-bydate-train/*/*
do 
  sed -E -e '/^$/,$d' -e 's/:.*//' -e '/^[[:space:]]/d' $file
done | sort | uniq -c | sort -nr
