
# Code from Chapter 6 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Code to run the decision tree on the Party dataset
from numpy import *
import dtree

tree = dtree.dtree()
party,classes,features = tree.read_data('party.data')
t=tree.make_tree(party,classes,features)
tree.printTree(t,' ')

print tree.classifyAll(t,party)

for i in range(len(party)):
    tree.classify(t,party[i])
