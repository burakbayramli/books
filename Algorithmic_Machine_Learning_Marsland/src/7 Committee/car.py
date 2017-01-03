
# Code from Chapter 7 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# An example of bagging on the Car Safety dataset
from numpy import *
import dtree
import bagging

tree = dtree.dtree()
bagger = bagging.bagger()
data,classes,features = tree.read_data('car.data')

train = data[::2][:]
test = data[1::2][:]
trainc = classes[::2]
testc = classes[1::2]

t=tree.make_tree(train,trainc,features)
out = tree.classifyAll(t,test)
tree.printTree(t,' ')

a = zeros(len(out))
b = zeros(len(out))
d = zeros(len(out))

for i in range(len(out)):
    if testc[i] == 'good' or testc[i]== 'v-good':
        b[i] = 1
        if out[i] == testc[i]:
            d[i] = 1
    if out[i] == testc[i]:
        a[i] = 1

print "Number correctly predicted",sum(a)
print "Number of testpoints ",len(a)
print "Percentage Accuracy ",sum(a)/len(a)*100.0
print ""
print "Number of cars rated as good or very good", sum(b)
print "Number correctly identified as good or very good",sum(d) 
print "Percentage Accuracy",sum(d)/sum(b)*100.0

c=bagger.bag(train,trainc,features,100)
out = bagger.bagclass(c,test)

a = zeros(len(out))
d = zeros(len(out))

for i in range(len(out)):
    if testc[i] == 'good' or testc[i]== 'v-good':
        if out[i] == testc[i]:
            d[i] = 1
    if out[i] == testc[i]:
        a[i] = 1
print "-----"
print "Number correctly predicted",sum(a)
print "Number of testpoints ",len(a)
print "Percentage Accuracy ",sum(a)/len(a)*100.0
print ""
print "Number of cars rated as good or very good", sum(b)
print "Number correctly identified as good or very good",sum(d) 
print "Percentage Accuracy",sum(d)/sum(b)*100.0
