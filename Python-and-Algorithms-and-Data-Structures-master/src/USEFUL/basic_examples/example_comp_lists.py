#!/usr/bin/env python

__author__ = "bt3"

a = [3, 4, 5, 6, 7]


# Filter elements greater than 4

# Bad:

b = []
for i in a:
    if i > 4:
        b.append(i)
print b

# Good:
print [i for i in a if i > 4]

# Or:
print filter(lambda x: x > 4, a)


# Add three to all list members:

# Bad
b = []
for i in range(len(a)):
    b.append(a[i] + 3)
print b

# Good:
print [i + 3 for i in a]

# Or:
print map(lambda i: i + 3, a)