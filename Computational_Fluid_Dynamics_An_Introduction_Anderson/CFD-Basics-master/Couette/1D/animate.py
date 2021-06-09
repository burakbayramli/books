import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

'''Load Dataset'''
fin = open('flow.txt', 'r')
NumOfPnt = int(fin.readline().strip())
y = np.array([float(e) for e in fin.readline().strip().split()])
NumOfStep = 0
t = []
record = []
cnt = 1

while(True):
    line = fin.readline()
    if not line:
        break
    
    marker = np.array([float(e) for e in line.strip().split()])
    t.append(marker[1])
    NumOfStep += 1
    cur_record = []
    for i in range(NumOfPnt):
        cur_record.append(float(fin.readline().strip()))
    record.append(np.array(cur_record))

fin.close()
t = np.array(t)
animation_data = np.array(record)

'''Functions'''
def draw(ax, data):
    global cnt

    bot = min(data)
    top = max(data)
    height = top - bot
    margin_height = 0.1 * height
    
    ax.cla()
    ax.plot(data, y, 'ro-', fillstyle='none')
    ax.set_ylim(bot-margin_height, top+margin_height)
    ax.text(0.1, 0.9, "Iter{}: t={}".format(cnt, t[cnt]))
    ax.set_xlabel(r'$u$')
    ax.set_ylabel(r'$Y$')
    ax.set_title("Couette Flow")

def update(data):
    global cnt, NumOfStep
    
    draw(ax, data)
    cnt = (cnt + 1) % NumOfStep

'''Plot data'''
fig = plt.figure()
ax = fig.add_subplot(111)
draw(ax, animation_data[0, :])
ret = animation.FuncAnimation(fig, update, animation_data)
plt.tight_layout()
plt.show()
