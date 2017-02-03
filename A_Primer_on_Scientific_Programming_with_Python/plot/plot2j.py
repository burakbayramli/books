from scitools.std import *

def f1(t):
    return t**2*exp(-t**2)

def f2(t):
    return t**2*f1(t)

t = linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

plot(t, y1, 'r-', t, y2, 'bo',
     legend=('t^2*exp(-t^2)', 't^4*exp(-t^2)'),
     hardcopy='tmp2.eps')

ax = gca()   # get current Axis object
ax.set(xlabel='t', ylabel='y',
       axis=[0, 4, -0.1, 0.6],
       title='Plotting two curves in the same plot')
show()  # show the plot again after ax.set actions

print ax.dump()
fig = gcf(); print fig.dump()
