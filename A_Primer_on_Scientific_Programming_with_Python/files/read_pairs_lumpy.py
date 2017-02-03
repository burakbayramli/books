from scitools.Lumpy import Lumpy

filetext = """\
(1.3,0)    (-1,2)    (3,-1.5)
(0,1)      (1,0)     (1,1)
(0,-0.01)  (10.5,-1) (2.5,-2.5)
"""
lines = filetext.splitlines()

lumpy = Lumpy()
lumpy.make_reference()
#lumpy_fig = False
lumpy_fig = True

pairs = []   # list of (n1, n2) pairs of numbers
for line in lines:
    words = line.split()
    for word in words:
        word = word[1:-1]  # strip off parenthesis
        n1, n2 = word.split(',')
        n1 = float(n1);  n2 = float(n2)
        pair = (n1, n2)
        pairs.append(pair)  # add 2-tuple to last row
    if lumpy_fig:
        lumpy.object_diagram()
        lumpy_fig = False  # no more figures

import pprint
pprint.pprint(pairs)

