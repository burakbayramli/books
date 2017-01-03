#!/usr/bin/env python
import re

# make some artificial output data from a program (just for testing):
sample_output = """
t=2.5  a: 1.0 6.2 -2.2   12 iterations and eps=1.38756E-05
t=4.25  a: 1.0 1.4   6 iterations and eps=2.22433E-05
>> switching from method AQ4 to AQP1
t=5  a: 0.9   2 iterations and eps=3.78796E-05
t=6.386  a: 1.0 1.1525   6 iterations and eps=2.22433E-06
>> switching from method AQP1 to AQ2
t=8.05  a: 1.0   3 iterations and eps=9.11111E-04
"""
lines = sample_output.split('\n')

# goal: process the lines and extract the t, iterations and eps
# data and store these in lists

# regex for a line, with the data to be extracted enclosed
# parenthesis (groups):
pattern = r't=(.*)\s+a:.*\s{3}(\d+) iterations and eps=(.*)'
# (this one extracts the t value with additional blanks, the
# next extracts exactly the t value):
pattern = r't=(.*)\s{2}a:.*\s+(\d+) iterations and eps=(.*)'

# too simple pattern:
#pattern = r't=(.*)\s+a:.*(\d+).*=(.*)'

# could compile first (for increased efficiency):
# line_pattern = re.compile(pattern)

# arrays for t, iterations and eps:
t = []; iterations = []; eps = []
# the output to be processed is stored in the list of lines:
for line in lines:
    match = re.search(pattern, line)
    # or if the pattern has been compiled:
    # m = line_pattern.search(line)
    if match:
        t.append(float(match.group(1)))
        iterations.append(int(match.group(2)))
        eps.append(float(match.group(3)))

print 't =', t
print 'iterations =', iterations
print 'eps =', eps

# can now plot iterations versus t or eps versus t directly,
# or we can make files out of the data

# produce two two-column files with (x,y) data for plotting:
f1 = open('iterations.tmp', 'w')
f2 = open('eps.tmp', 'w')
for i in range(len(t)):
    f1.write('%g %g' % (t[i], iterations[i]))
    f2.write('%g %g' % (t[i], eps[i]))
f1.close(); f2.close()

