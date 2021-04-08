import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation


def relaxation(a, b, x):
    return (1.0 - x) * a + x * b

'''Load Dataset'''
fin = open('history_at_15_5.txt', 'r')
y = np.array([float(e) for e in fin.readline().strip().split()])

NumOfStep = 0
u = []
v = []
d = []

while(True):
    line = fin.readline()
    if not line:
        break
    
    cur_u = np.array([float(e) for e in line.strip().split()])
    u.append(cur_u)
    
    line = fin.readline()
    cur_v = np.array([float(e) for e in line.strip().split()])
    v.append(cur_v)

    line = fin.readline()
    cur_d = float(line.strip().split()[0])
    d.append(cur_d)

    NumOfStep += 1

fin.close()

d = np.array(d)
u = np.array(u)
v = np.array(v)

u_min = 0.0
u_max = 1.0

v_min = 0.0
v_max = 0.5

'''Convergency history of mass flux'''
fig_d = plt.figure()
ax = fig_d.add_subplot(111)
ax.plot(range(NumOfStep), d, '.-')
ax.set_xlabel('Iteration')
ax.set_ylabel('Mass flux')
plt.tight_layout()
plt.show()


'''Convergency history of u at i=15'''
cnt1 = 1
def draw_u(ax, data):
    global cnt1

    ax.cla()
    ax.plot([u_min, u_max], [y[0], y[-1]], '--')
    ax.plot(data, y, 'ro-', fillstyle='none')
    ax.text(u_min, y[-1], "Iter:{}".format(cnt1))
    ax.text(relaxation(u_min, u_max, 0.75), relaxation(y[0], y[-1], 0.25), 'Profile at i=15')
    ax.set_xlabel(r'$u/(m/s)$')
    ax.set_ylabel(r'$Y/m$')
    ax.set_title("Couette Flow")

def update_u(data):
    global cnt1, NumOfStep

    draw_u(ax, data)
    cnt1 = (cnt1 + 1) % NumOfStep

fig_u = plt.figure()
ax = fig_u.add_subplot(111)
draw_u(ax, u[0, :])
ret = animation.FuncAnimation(fig_u, update_u, u)
plt.tight_layout()
plt.show()


'''Convergency history of v at i=15'''
cnt2 = 1
def draw_v(ax, data):
    global cnt2

    ax.cla()
    ax.plot(data, y, 'ro-', fillstyle='none')
    ax.set_xlim(-v_max, v_max)
    ax.text(relaxation(v_min, v_max, 0.75), y[-1], "Iter:{}".format(cnt2))
    ax.text(relaxation(v_min, v_max, -0.75), relaxation(y[0], y[-1], 0.25), 'Profile at i=15')
    ax.set_xlabel(r'$v/(m/s)$')
    ax.set_ylabel(r'$Y/m$')
    ax.set_title("Couette Flow")

def update_v(data):
    global cnt2, NumOfStep

    draw_v(ax, data)
    cnt2 = (cnt2 + 1) % NumOfStep

fig_v = plt.figure()
ax = fig_v.add_subplot(111)
draw_v(ax, v[0, :])
ret = animation.FuncAnimation(fig_v, update_v, v)
plt.tight_layout()
plt.show()
