# read in file and strip trailing empty space 
s = open('dna.txt').read().strip()
len(s)
s

# method 1 - use built-in count method of strings
s.count('A')
s.count('C')
s.count('T')
s.count('G')

# method 2 - use sum and a generator comprehension
sum(1 for x in s if x=='A')
sum(1 for x in s if x=='C')
sum(1 for x in s if x=='T')
sum(1 for x in s if x=='G')

# method 3 - use a defaultdict with default value = 0
import collections
counter = collections.defaultdict(int)
for x in s:
    counter[x] += 1
print counter

# method 4 - use vectorized equality operation after conversion of
# string to numpy character array
import numpy
sum(numpy.array(s, 'c')=='A')
sum(numpy.array(s, 'c')=='C')
sum(numpy.array(s, 'c')=='T')
sum(numpy.array(s, 'c')=='G')


