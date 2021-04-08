import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

'''Load Dataset'''
fin = open('flow.txt', 'r')
NumOfPnt = int(fin.readline().strip())
x = np.array([float(e) for e in fin.readline().strip().split()])
A = np.array([float(e) for e in fin.readline().strip().split()])
NumOfStep = 0
t = []
record = []

while(True):
    line = fin.readline()
    if not line:
        break
    
    marker = np.array([float(e) for e in line.strip().split()])
    t.append(marker[1])
    NumOfStep += 1
    cur_record = []
    for i in range(NumOfPnt):
        data = np.array([float(e) for e in fin.readline().strip().split()])
        cur_record.append(data)
    record.append(np.array(cur_record))

fin.close()
t = np.array(t)
animation_data = np.array(record)

'''Plot data'''
fig = plt.figure()
ax1 = fig.add_subplot(231)
ax1.set_ylabel(r'$\rho$')
ax2 = fig.add_subplot(232)
ax2.set_ylabel(r'$V$')
ax3 = fig.add_subplot(233)
ax3.set_xlabel('X')
ax3.set_ylabel(r'$T$')
ax4 = fig.add_subplot(234)
ax4.set_xlabel('X')
ax4.set_ylabel(r'$P$')
ax5 = fig.add_subplot(235)
ax5.set_xlabel('X')
ax5.set_ylabel(r'$Ma$')
ax6 = fig.add_subplot(236)
ax6.set_xlabel('X')
ax6.set_ylabel(r'$mdot$')

line11, = ax1.plot(x, animation_data[0, :, 0], 'ro', fillstyle='none')
line21, = ax2.plot(x, animation_data[0, :, 1], 'ro', fillstyle='none')
line31, = ax3.plot(x, animation_data[0, :, 2], 'ro', fillstyle='none')
line41, = ax4.plot(x, animation_data[0, :, 3], 'ro', fillstyle='none')
line51, = ax5.plot(x, animation_data[0, :, 4], 'ro', fillstyle='none')
line61, = ax6.plot(x, animation_data[0, :, 5], 'ro', fillstyle='none')

def helper(ax, line, data):
    bot = min(data)
    top = max(data)
    height = top - bot
    margin_height = 0.1 * height
    ax.set_ylim(bot-margin_height, top+margin_height)
    line.set_ydata(data)

cnt = 1

def update(data):
    global cnt, NumOfStep

    print("Iter{}: t={}".format(cnt, t[cnt]))
    helper(ax1, line11, data[:, 0])
    helper(ax2, line21, data[:, 1])
    helper(ax3, line31, data[:, 2])
    helper(ax4, line41, data[:, 3])
    helper(ax5, line51, data[:, 4])
    helper(ax6, line61, data[:, 5])
    cnt += 1
    cnt %= NumOfStep

ret = animation.FuncAnimation(fig, update, animation_data)

plt.tight_layout()
plt.show()
