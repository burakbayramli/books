from __future__ import division
from __future__ import print_function
import datetime as dt
import datetime as dt 
import sys
from pylab import *
from numpy import *
# End Imports


x = zeros(4,[('date','int'),('ret','float')])
x = zeros(4,{'names': ('date','ret'), 'formats': ('int', 'float')})
x

x['date']
x['ret']

t = dtype([('var1','f8'), ('var2','i8'), ('var3','u8')])

ba = dtype([('bid','f8'), ('ask','f8')])
t = dtype([('date', 'O8'), ('prices', ba)])
data = zeros(2,t)

x = array([dt.datetime.now()])
# The size in bytes
print(x.dtype.itemsize)
# The name and description
print(x.dtype.descr)

t = dtype([('date', 'u4'), ('time', 'u4'),
           ('size', 'u4'), ('price', 'f8'),
           ('g127', 'u2'), ('corr', 'u2'),
           ('cond', 'S2'), ('ex', 'S2')])
taqData = zeros(10, dtype=t)
taqData[0] = (20120201,120139,1,53.21,0,0,'','N')

t = dtype([('datetime', 'O8'), ('size', 'u4'), ('price', 'f8'),
           ('g127', 'u2'), ('corr', 'u2'), ('cond', 'S2'), ('ex', 'S2')])
taqData = zeros(10, dtype=t)
taqData[0] = (dt.datetime(2012,2,1,12,01,39),1,53.21,0,0,'','N')

x = zeros((4,1),[('date','int'),('ret','float')])
y = rec.array(x)
y.date
y.date[0]

