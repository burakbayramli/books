# example of encoding a vector as a chromosome

from __future__ import print_function

import numpy as np
import encode_decode as ed

rs = np.array([0.125, 0.35, 0.9])

m = 5

c = None

for r in rs:
    y = ed.encode(r, m)
    if c is None:
        c = y
    else:
        c = np.hstack((c,y))

print(c.astype(int))


# now decode:
c = c.reshape(len(rs), m)

for row in c:
    print(ed.decode(row))




