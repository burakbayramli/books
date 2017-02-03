import numpy as np
import matplotlib.pyplot as plt

raw_input=lambda  x: None

def f(t):
    return t**2*np.exp(-t**2)

t = np.linspace(0, 3, 51)    # 51 points between 0 and 3
y = np.zeros(len(t))         # allocate y with float elements
y = f(t)                     # compute all f values at once
plt.plot(t, y)

plt.savefig('tmp1.eps') # produce PostScript
plt.savefig('tmp1.png') # produce PNG

plt.show()
raw_input('Press the Return key to quit: ')

#plt.plot(t, y)
#plt.legend(['t^2*exp(-t^2)'])
plt.plot(t, y, label='t^2*exp(-t^2)')
plt.xlabel('t')
plt.ylabel('y')
plt.axis([0, 3, -0.05, 0.6])   # [tmin, tmax, ymin, ymax]
plt.title('My First Matplotlib Demo')
plt.show()
plt.savefig('tmp2.eps') # produce PostScript
raw_input('Press the Return key to quit: ')


# Multiple curves

def f1(t):
    return t**2*np.exp(-t**2)

def f2(t):
    return t**2*f1(t)

t = np.linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

plt.plot(t, y1, 'r-')
plt.plot(t, y2, 'bo')
plt.xlabel('t')
plt.ylabel('y')
plt.legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
plt.title('Plotting two curves in the same plot')
plt.savefig('tmp3.eps')
plt.show()

# multiple plots
plt.figure()
plt.subplot(2, 1, 1)
t = np.linspace(0, 3, 51)
y1 = f1(t)
y2 = f2(t)

plt.plot(t, y1, 'r-', t, y2, 'bo')
plt.xlabel('t')
plt.ylabel('y')
plt.axis([t[0], t[-1], min(y2)-0.05, max(y2)+0.5])
plt.legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
plt.title('Top figure')

plt.subplot(2, 1, 2)
t3 = t[::4]
y3 = f2(t3)

plt.plot(t, y1, 'b-', t3, y3, 'ys')
plt.xlabel('t')
plt.ylabel('y')
plt.axis([0, 4, -0.2, 0.6])
plt.legend(['t^2*exp(-t^2)', 't^4*exp(-t^2)'])
plt.title('Bottom figure')
plt.savefig('tmp4.eps')
plt.show()

raw_input('Press the Return key to quit: ')
