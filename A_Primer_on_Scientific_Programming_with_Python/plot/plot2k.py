from scitools.std import *   # for curve plotting

def f1(t):
    return t**2*exp(-t**2)

def f2(t):
    return t**2*f1(t)

t = linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

# a mix of Matlab style and options in plot commands

plot(t, y1, 'r-', xlabel='t', ylabel='y',
     axis=[0, 4, -0.1, 0.6])

figure()  # new figure

plot(t, y2, 'bo', xlabel='t', ylabel='y')

# pause so the user can really see that we add features
# to the plot below:
input('Press Return: ')

figure(1)  # go back to first figure
title('One curve')
legend('t^2*exp(-t^2)')
show()
hardcopy('tmp2_1.eps')

figure(2)  # go to second figure
title('Another curve')
hardcopy('tmp2_2.eps')
show()

figure()  # new, third figure
# plot y1 and y2 as two axis in the same figure:
subplot(2, 1, 1)
plot(t, y1, xlabel='t', ylabel='y')
subplot(2, 1, 2)
plot(t, y2, xlabel='t', ylabel='y')
title('A figure with two plots')
show()
#hardcopy('tmp2_3.eps')  # illegal in multiplot mode



