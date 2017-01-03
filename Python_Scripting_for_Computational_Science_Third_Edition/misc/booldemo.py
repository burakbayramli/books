#!/usr/bin/env python
b=0
print 'b=0; bool(b):', bool(b)
print 'b==False:', b==False
b=1
print 'b=1; bool(b):', bool(b)
print 'b==True', b==True
b=10
print 'b=10; bool(b):', bool(b), '(true integer value)'
print 'b==True:', b==True, '(only 0 and 1 make sense in comparison with bool)'
print 'b=False:', b==False
print 'b==0:', b==0
b=True
print 'b=True; b==1:', b==1
print 'b==10', b==10, '(only 0 and 1 make sense in this context)'

