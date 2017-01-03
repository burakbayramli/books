"""Solutions for 'Graphics' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('graphics_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from numpy import diff, log, hstack, sqrt, array, var, mean, logical_and, \
    zeros, ones, dot, floor, amax, arange, argwhere, int32, squeeze
from numpy.linalg import lstsq
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.mlab as ml
import datetime as dt
# <demo> --- stop ---
# Loading and transforming the data
SP500data = ml.csv2rec('SP500.csv')
FTSEdata = ml.csv2rec('FTSE.csv')

SP500 = SP500data['adj_close']
SP500dates = SP500data['date']
FTSE = FTSEdata['adj_close']
FTSEdates = FTSEdata['date']

SweekDays = [dt.datetime.weekday(d) for d in SP500dates]
FweekDays = [dt.datetime.weekday(d) for d in FTSEdates]

lastFriday = mdates.date2num(dt.date(2012,3,16))
weeks = floor((lastFriday-min(mdates.date2num(SP500dates)))/7)
firstFriday = lastFriday - weeks*7
fridays = arange(firstFriday,lastFriday+1,7)

FnumDates = mdates.date2num(FTSEdates)
SnumDates = mdates.date2num(SP500dates)

fridayLocs = zeros((len(fridays),2),dtype=int32)
for i in xrange(len(fridays)):
    s = amax(argwhere(SnumDates<=fridays[i]))
    f = amax(argwhere(FnumDates<=fridays[i]))
    fridayLocs[i] = array([s,f])
    
dates = mdates.num2date(fridays)    
prices = array([SP500[fridayLocs[:,0]],FTSE[fridayLocs[:,1]]])
prices = prices.T

SP500rets = diff(log(prices[:,0]))
FTSErets = diff(log(prices[:,1]))

# <demo> --- stop ---
# Exercise 1
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(SP500dates,SP500)
fmt = mdates.DateFormatter('%Y')
ax.xaxis.set_major_formatter(fmt)
fig.autofmt_xdate()
plt.draw()    
# <demo> --- stop ---
# Exercise 2
fig = plt.figure()
ax = fig.add_subplot(111)
ax.hist(SP500rets)
plt.draw()    

fig = plt.figure()
ax = fig.add_subplot(111)
ax.hist(SP500rets,bins=20)
plt.draw()    
# <demo> --- stop ---
# Exercise 3
bands = [(-1.0,-.02),(-.02,0.0),(0.0,.02),(.02,1.0)]
percs = zeros(len(bands))
i = 0
for b in bands:
    percs[i] = mean(logical_and(SP500rets>b[0],SP500rets<=b[1]))
    i+=1
    
fig = plt.figure()
ax = fig.add_subplot(111)
labels = ['<-2%','>-2% and <0','>0 and <2%','>2%']
ax.pie(percs,labels=labels)
plt.draw()

# <demo> --- stop ---
# Exercise 4
fig = plt.figure()
ax = fig.add_subplot(111)
ax.scatter(SP500rets,FTSErets)
ax.set_xlabel('S&P 500 returns')
ax.set_ylabel('FTSE 100 returns')
plt.draw()

# <demo> --- stop ---
# Exercise 5
SP500rets.shape = -1,1
FTSErets.shape = -1,1
X = hstack((ones(SP500rets.shape),SP500rets))
y = FTSErets
out = lstsq(X,y)
b = out[0]

fig = plt.figure()
ax = fig.add_subplot(111)
ax.scatter(SP500rets,FTSErets)
ax.set_xlabel('S&P 500 returns')
ax.set_ylabel('FTSE 100 returns')
ax.hold(True)
x = ax.get_xlim()
x = array(x)
x.shape = -1,1
x = hstack((ones(x.shape),x))
fit = dot(x,b)
b = squeeze(b)
ax.plot(x[:,1],fit, color='#800000')
ax.text(.2,.2,'y = {:0.2f} + {:0.2f}x'.format(b[0],b[1]))
ax.hold(False)
plt.draw()    

# <demo> --- stop ---
# Exercise 6
T = SP500rets.size
EWMA = zeros((T,2))
r = hstack((SP500rets,FTSErets))
EWMA[0] = var(r,axis=0)
for i in xrange(1,T):
    EWMA[i] = .97 * EWMA[i-1] + .03 * r[i-1]**2
EWMA = 100*sqrt(252*EWMA)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(dates[1:],EWMA[:,0], label = 'S&P 500 EWMA Vol (Annualized)')
ax.hold(True)
ax.plot(dates[1:],EWMA[:,1], label = 'FTSE 100 EWMA Vol (Annualized)')
ax.legend(loc=0)
ax.set_title('Annualized Volatility (%)')
fmt = mdates.DateFormatter('%Y')
ax.xaxis.set_major_formatter(fmt)
fig.autofmt_xdate()
plt.show()    