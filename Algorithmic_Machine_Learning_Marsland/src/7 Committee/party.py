
# Code from Chapter 7 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# Comparison of stumping and bagging on the Party dataset
from numpy import *
import dtw
import bagging

tree = dtw.dtree()
bagger = bagging.bagger()
party,classes,features = tree.read_data('../6 Trees/party.data')

w = ones((shape(party)[0]),dtype = float)/shape(party)[0]

t=tree.make_tree(party,w,classes,features,1)
#tree.printTree(t,' ')

print "Tree Stump Prediction"
print tree.classifyAll(t,party)
print "True Classes"
print classes

c=bagger.bag(party,classes,features,20)
print "Bagged Results"
print bagger.bagclass(c,party)
