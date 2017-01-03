from __future__ import division
from __future__ import print_function
from matplotlib import rc
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d import Axes3D 
from numpy import linspace, meshgrid, mat, zeros, shape, sqrt
import datetime as dt
import matplotlib.cm as cm
import matplotlib.dates as mdates
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
import numpy as np
import numpy.linalg as linalg
import numpy.random as rnd
import scipy.stats as stats
import sys
from pylab import *
from numpy import *
# End Imports



y = randn(100)
plt.plot(y)

plt.plot(y,'g--')

x = cumsum(rand(100))
plt.plot(x,y,'r-')

plt.plot(x,y,alpha = 0.5, color = '#FF7F00', \
   label = 'Line Label', linestyle = '-.', \
   linewidth = 3, marker = 'o', markeredgecolor = '#000000', \
   markeredgewidth = 2, markerfacecolor = '#FF7F00', \
   markersize=30)

h = plot(randn(10))
getp(h)
setp(h, 'alpha')
setp(h, 'color')
setp(h, 'linestyle')
setp(h, 'linestyle', '--') # Change the line style

z = randn(100,2)
z[:,1] = 0.5*z[:,0] + sqrt(0.5)*z[:,1]
x=z[:,0]
y=z[:,1]
plt.scatter(x,y)

plt.scatter(x,y, s = 60,  c = '#FF7F00', marker='s', \
    alpha = .5, label = 'Scatter Data')

s = exp(exp(exp(rand(100))))
s = 200 * s/amax(s)
s[s<1]=1
plt.scatter(x,y, s = s, c = '#FF7F00', marker='s', \
    label = 'Scatter Data')

y = rand(5)
x = arange(5)
plt.bar(x,y)

plt.bar(x,y, width = 0.5, color = '#FF7F00', \
    edgecolor = '#000000', linewidth = 5)

colors = ['#FF0000','#FFFF00','#00FF00','#00FFFF','#0000FF']
plt.barh(x, y, height = 0.5, color = colors, \
    edgecolor = '#000000', linewidth = 5)

y = rand(5)
y = y/sum(y)
y[y<.05] = .05
plt.pie(y)

explode = np.array([.2,0,0,0,0])
colors = ['#FF0000','#FFFF00','#00FF00','#00FFFF','#0000FF']
labels = ['One','Two','Three','Four','Five']
plt.pie(y, explode = explode, colors = colors, \
   labels = labels, autopct = '%2.0f', shadow = True)

x = randn(1000)
plt.hist(x, bins = 30)

plt.hist(x, bins = 30, cumulative=True, color='#FF7F00')

fig = plt.figure()
# Add the subplot to the figure
# Panel 1
ax = fig.add_subplot(2,2,1)
y = np.random.randn(100)
plt.plot(y)
ax.set_title('1')
# Panel  2
y = np.random.rand(5)
x = np.arange(5)
ax = fig.add_subplot(2,2,2)
plt.bar(x,y)
ax.set_title('2')
# Panel 3
y = np.random.rand(5)
y = y/sum(y)
y[y<.05] = .05
ax = fig.add_subplot(2,2,3)
plt.pie(y)
ax.set_title('3')
# Panel 4
z = np.random.randn(100,2)
z[:,1] = 0.5* z[:,0] + np.sqrt(0.5) * z[:,1]
x=z[:,1]
y=z[:,1]
ax = fig.add_subplot(2,2,4)
plt.scatter(x,y)
ax.set_title('4')
plt.draw()

fig = plt.figure()
ax = fig.add_subplot(111)
ax.hist(x, bins = 30,label = 'Empirical')
xlim = ax.get_xlim()
ylim = ax.get_ylim()
pdfx = np.linspace(xlim[0],xlim[1],200)
pdfy = stats.norm.pdf(pdfx)
pdfy = pdfy / pdfy.max() * ylim[1]
plt.hold(True)
plt.plot(pdfx,pdfy,'r-',label = 'PDF')
ax.set_ylim((ylim[0],1.2*ylim[1]))
plt.legend()
hold(False)

x = cumsum(randn(100,3), axis = 0)
plt.plot(x[:,0],'b-',label = 'Series 1')
plt.hold(True)
plt.plot(x[:,1],'g-.',label = 'Series 2')
plt.plot(x[:,2],'r:',label = 'Series 3')
plt.legend()
plt.title('Basic Legend')

plt.plot(x[:,0],'b-',label = 'Series 1')
plt.hold(True)
plt.plot(x[:,1],'g-.',label = 'Series 2')
plt.plot(x[:,2],'r:',label = 'Series 3')
plt.legend(loc = 0, frameon = False, title = 'The Legend')
plt.title('Improved Legend')

# Simulate data
T = 2000
x = []
for i in xrange(T):
    x.append(dt.datetime(2012,3,1)+dt.timedelta(i,0,0))
y = np.cumsum(rnd.randn(T))

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x,y)
plt.draw()

fig.autofmt_xdate()
plt.draw()

T = 100
x = []
for i in xrange(T):
    x.append(dt.datetime(2012,3,1)+dt.timedelta(i,0,0))
y = np.cumsum(rnd.randn(T))

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x,y)
plt.draw()

fig.autofmt_xdate()
plt.draw()

months = mdates.MonthLocator()
ax.xaxis.set_major_locator(months)
fmt = mdates.DateFormatter('%b %Y')
ax.xaxis.set_major_formatter(fmt)
fig.autofmt_xdate()
plt.draw()

xlim = list(ax.get_xlim())
xlim[0] = mdates.date2num(dt.datetime(2012,3,1))
ax.set_xlim(xlim)
plt.draw()

# Reading the data
# csv2rec for simplicity
recessionDates = mlab.csv2rec('USREC.csv',skiprows=0)
capacityUtilization = mlab.csv2rec('TCU.csv')
d1 = set(recessionDates['date'])
d2 = set(capacityUtilization['date'])
# Find the common dates
commonDates = d1.intersection(d2)
commonDates = list(commonDates)
commonDates.sort()
# And the first date
firstDate = min(commonDates)
# Find the data after the first date
plotData = capacityUtilization[capacityUtilization['date']>firstDate]
shadeData = recessionDates[recessionDates['date']>firstDate]

# The shaded plot
x = plotData['date']
y = plotData['value']
# z is the shading values, 1 or 0
z = shadeData['value']!=0
# Figure
fig = plt.figure()
ax = fig.add_subplot(111)
plt.plot_date(x,y,'r-')
limits = plt.axis()
font = { 'fontname':'Times New Roman', 'fontsize':14 }
ax.fill_between(x, limits[2], limits[3], where=z, edgecolor='#BBBBBB', \
    facecolor='#BBBBBB', alpha=0.5)
plt.axis(ymin=limits[2])
ax.set_title('Capacity Utilization',font)
xl = ax.get_xticklabels()
for label in xl:
    label.set_fontname('Times New Roman')
    label.set_fontsize(14)
    label.set_rotation(45)
yl = ax.get_yticklabels()
for label in yl:
    label.set_fontname('Times New Roman')
    label.set_fontsize(14)
plt.draw()

rc('text', usetex=True)
rc('font', family='serif')
y = 50*np.exp(.0004 + np.cumsum(.01*np.random.randn(100)))
plt.plot(y)
plt.xlabel(r'time ($\tau$)')
plt.ylabel(r'Price',fontsize=16)
plt.title(r'Geometric Random Walk: $d\ln p_t = \mu dt + \sigma dW_t$',fontsize=16)
rc('text', usetex=False)

x = np.linspace(0,6*np.pi,600)
z = x.copy()
y = np.sin(x)
x=  np.cos(x)
fig = plt.figure()
ax = Axes3D(fig) # Different usage
ax.plot(x, y, zs=z, label='Spiral')
ax.view_init(15,45)
plt.draw()

x = linspace(-3,3,100)
y = linspace(-3,3,100)
x,y = meshgrid(x,y)
z = mat(zeros(2))
p = zeros(shape(x))
R = matrix([[1,.5],[.5,1]])
Rinv = linalg.inv(R)
for i in xrange(len(x)):
    for j in xrange(len(y)):
        z[0,0] = x[i,j]
        z[0,1] = y[i,j]
        p[i,j] = 1.0/(2*pi)*sqrt(linalg.det(R))*exp(-(z*Rinv*z.T)/2)

fig = plt.figure()
ax = Axes3D(fig)
ax.plot_wireframe(x, y, p, rstride=5, cstride=5)
ax.view_init(29,80)
plt.draw()

fig = plt.figure()
ax = Axes3D(fig)
ax.plot_surface(x, y, p, rstride=5, cstride=5, cmap=cm.jet)
ax.view_init(29,80)
plt.draw()

fig = plt.figure()
ax = fig.gca()
ax.contour(x,y,p)
plt.draw()

plt.plot(randn(10,2))
savefig('figure.pdf') # PDF export
savefig('figure.png') # PNG export
savefig('figure.svg') # Scalable Vector Graphics export

plt.plot(randn(10,2))
savefig('figure.png', dpi = 600) # High resolution PNG export

