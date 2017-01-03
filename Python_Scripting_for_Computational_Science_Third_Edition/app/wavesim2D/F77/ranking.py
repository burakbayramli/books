#!/usr/bin/env python
import sys, re, os
try:
    res = open(sys.argv[1],'r').readlines()
except:
    print "Usage %s output" % sys.argv[0]; sys.exit(1)
table = []
for line in res:
    if line[0:8] == "CPU-time":
        table.append(line)

# sort wrt 2nd column
def sort_CPUtimes(a, b):
    "a and b are two lines"
    cpu_a = float(a.split()[1])
    cpu_b = float(b.split()[1])
    if cpu_a < cpu_b:
        return -1
    elif cpu_b < a:
        return 1
    else:
        return 0

for line in table:  print line,

stable = table[:]
stable.sort(sort_CPUtimes)
print "\n\n\nsorted version\n"
for line in stable:  print line,
