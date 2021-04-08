import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation


def relaxation(a, b, x):
    return (1.0 - x) * a + x * b

'''Load Dataset'''
fin = open('history.txt', 'r')
u_inf, v_inf, p_inf, T_inf = [float(e) for e in fin.readline().strip().split()]
x = np.array([float(e) for e in fin.readline().strip().split()])
y = np.array([float(e) for e in fin.readline().strip().split()])
Nx = len(x)
Ny = len(y)

p_bottom = []
p_out = []
T_out = []
U_out = []
Ma_out = []

NumOfStep = 0
while(True):
    line = fin.readline()
    if not line:
        break
    
    cur_pb = np.array([float(e) for e in line.strip().split()])
    p_bottom.append(cur_pb)
    
    line = fin.readline()
    cur_po = np.array([float(e) for e in line.strip().split()])
    p_out.append(cur_po)

    line = fin.readline()
    cur_To = np.array([float(e) for e in line.strip().split()])
    T_out.append(cur_To)

    line = fin.readline()
    cur_Uo = np.array([float(e) for e in line.strip().split()])
    U_out.append(cur_Uo)

    line = fin.readline()
    cur_Mao = np.array([float(e) for e in line.strip().split()])
    Ma_out.append(cur_Mao)

    NumOfStep += 1

fin.close()

'''Data processing'''
p_bottom = np.array(p_bottom) / p_inf
p_out = np.array(p_out) / p_inf
T_out = np.array(T_out) / T_inf
U_out = np.array(U_out) / u_inf
Ma_out = np.array(Ma_out)

'''Animation of pressure at bottom'''
fig_bottom = plt.figure()
ax_pb = fig_bottom.add_subplot(111)
cnt1 = 1
def draw_pb(ax, data):
    global cnt1

    ax.cla()
    ax.plot(x, data, 'ro-', fillstyle='none')
    ax.text(relaxation(x[0], x[-1], 0.9), max(data), "Iter:{}".format(cnt1))
    ax.set_xlabel(r'$x/m$')
    ax.set_ylabel(r'$p/p_{\infty}$ at bottom')
    ax.set_title("Supersonic plate")

def update_pb(data):
    global cnt1, NumOfStep

    draw_pb(ax_pb, data)
    cnt1 = (cnt1 + 1) % NumOfStep

draw_pb(ax_pb, p_bottom[0])
ret = animation.FuncAnimation(fig_bottom, update_pb, p_bottom)
plt.show()

'''Animation of variables at outlet'''
fig_outlet = plt.figure()
ax_po = fig_outlet.add_subplot(221)
ax_To = fig_outlet.add_subplot(222)
ax_Uo = fig_outlet.add_subplot(223)
ax_Mao = fig_outlet.add_subplot(224)
cnt2 = 1
def draw_outlet(data):
    global cnt2

    po_data = data[0]
    ax_po.cla()
    ax_po.plot(po_data, y, 'ro-', fillstyle='none')
    ax_po.text(relaxation(min(po_data), max(po_data), 0.9), y[-1], "Iter:{}".format(cnt2))
    ax_po.set_ylabel(r'$y/m$')
    ax_po.set_xlabel(r'$p/p_{\infty}$ at outlet')

    To_data = data[1]
    ax_To.cla()
    ax_To.plot(To_data, y, 'ro-', fillstyle='none')
    ax_To.text(relaxation(min(To_data), max(To_data), 0.9), y[-1], "Iter:{}".format(cnt2))
    ax_To.set_ylabel(r'$y/m$')
    ax_To.set_xlabel(r'$T/T_{\infty}$ at outlet')

    Uo_data = data[2]
    ax_Uo.cla()
    ax_Uo.plot(Uo_data, y, 'ro-', fillstyle='none')
    ax_Uo.text(relaxation(min(Uo_data), max(Uo_data), 0.0), y[-1], "Iter:{}".format(cnt2))
    ax_Uo.set_ylabel(r'$y/m$')
    ax_Uo.set_xlabel(r'$U/U_{\infty}$ at outlet')

    Mao_data = data[3]
    ax_Mao.cla()
    ax_Mao.plot(Mao_data, y, 'ro-', fillstyle='none')
    ax_Mao.text(relaxation(min(Mao_data), max(Mao_data), 0.0), y[-1], "Iter:{}".format(cnt2))
    ax_Mao.set_ylabel(r'$y/m$')
    ax_Mao.set_xlabel(r'$Ma$ at outlet')

def update_outlet(data):
    global cnt2, NumOfStep

    draw_outlet(data)
    cnt2 = (cnt2 + 1) % NumOfStep

outlet_data = np.zeros((NumOfStep, 4, Ny))
for i in range(NumOfStep):
    outlet_data[i][0] = p_out[i]
    outlet_data[i][1] = T_out[i]
    outlet_data[i][2] = U_out[i]
    outlet_data[i][3] = Ma_out[i]
draw_outlet(outlet_data[0])
ret = animation.FuncAnimation(fig_outlet, update_outlet, outlet_data)
plt.show()
