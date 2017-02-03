from matplotlib.pylab import *
raw_input=lambda  x: None

def f(t):
    return t**2*exp(-t**2)

t = linspace(0, 3, 51)    # 51 points between 0 and 3
y = zeros(len(t))         # allocate y with float elements
for i in xrange(len(t)):
    y[i] = f(t[i])

plot(t, y)
show()
raw_input('Press the Return key to quit: ')

# vectorized version:
y = f(t)                  # compute all f values at once
# or
y = t**2*exp(-t**2)
plot(t, y)

savefig('tmp1.eps') # produce PostScript
savefig('tmp1.png') # produce PNG
raw_input('Press the Return key to quit: ')
show()

plot(t, y)
legend(['t^2*exp(-t^2)'])
xlabel('t')
ylabel('y')
axis([0, 3, -0.05, 0.6])   # [tmin, tmax, ymin, ymax]
title('My First Matplotlib Demo')
savefig('tmp2.eps') # produce PostScript
show()
raw_input('Press the Return key to quit: ')


# Multiple curves

def f1(t):
    return t**2*exp(-t**2)

def f2(t):
    return t**2*f1(t)

t = linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

plot(t, y1, 'r-')
hold('on')
#hold(True)
plot(t, y2, 'bo')
xlabel('t')
ylabel('y')
legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
title('Plotting two curves in the same plot')
savefig('tmp3.eps')
show()

# multiple plots
figure()
subplot(2, 1, 1)
t = linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

plot(t, y1, 'r-', t, y2, 'bo')
xlabel('t')
ylabel('y')
axis([t[0], t[-1], min(y2)-0.05, max(y2)+0.5])
legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
title('Top figure')

subplot(2, 1, 2)
t3 = t[::4]
y3 = f2(t3)

plot(t, y1, 'b-', t3, y3, 'ys')
legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
xlabel('t')
ylabel('y')
axis([0, 4, -0.2, 0.6])
title('Bottom figure')
savefig('tmp4.eps')
show()

raw_input('Press the Return key to quit: ')
