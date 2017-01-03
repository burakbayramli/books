#!/usr/bin/python

import sys, os, re

# read the local file from standard input
input_file=sys.stdin.readline()

# extract the filename from the file
filename = os.path.basename(input_file)

# extract the date from the filename
date=re.search(r'([0-9]{4}\-[0-9]{2}\-[0-9]{2})', filename).group(1)

# construct our destination HDFS file
hdfs_dest="hdfs:/slurper/in/%s/%s" % (date, filename)

# write it to standard output
print hdfs_dest,
