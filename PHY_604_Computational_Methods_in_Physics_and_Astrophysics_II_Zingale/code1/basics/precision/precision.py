from __future__ import print_function

x = 1.0

eps = 1.0

while x + eps != x:
    eps = eps/2.0

# machine precision is 2*eps, since that was the last value for which
# 1 + eps was not 1
print(2*eps)

