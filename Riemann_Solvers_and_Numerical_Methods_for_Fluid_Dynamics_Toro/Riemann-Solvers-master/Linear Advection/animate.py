import os
import sys
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

def load_data(f):
    fin = open(f, 'r')
    NumOfStep, NumOfPnt = fin.readline().strip().split()
    
    NumOfStep = int(NumOfStep)
    NumOfPnt = int(NumOfPnt)

    x = np.array([float(c) for c in fin.readline().strip().split()])

    all_data = []
    for _ in range(NumOfStep):
        cur_data = [float (c) for c in fin.readline().strip().split()]
        all_data.append(cur_data)
    animation_data = np.array(all_data)

    fin.close()

    return NumOfStep, NumOfPnt, x, animation_data

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python3 animate.py data1.txt data2.txt")
        exit(-1)

    data1_file_name = sys.argv[1]
    data2_file_name = sys.argv[2]
    if not os.path.exists(data1_file_name):
        print("{:s} not exist!".format(data1_file_name))
        exit(-2)
    if not os.path.exists(data2_file_name):
        print("{:s} not exist!".format(data2_file_name))
        exit(-3)

    case1_name = data1_file_name[:-4]
    case2_name = data2_file_name[:-4]

    NumOfStep, NumOfPnt, x, data1_sol = load_data(data1_file_name)
    _, _, _, data2_sol = load_data(data2_file_name)

    fig = plt.figure()
    
    ax = fig.add_subplot(111)
    ax.set_ylabel(r'$u$')
    ax.set_xlabel('X')
    line11, = ax.plot(x, data1_sol[0, :], '-', label=case1_name)
    line12, = ax.plot(x, data2_sol[0, :], '-.', label=case2_name)
    ax.legend()

    def update(k):
        line11.set_data(x, data1_sol[k, :])
        line12.set_data(x, data2_sol[k, :])
        ax.set_title('t={}'.format(k))
        return line11, line12

    a = animation.FuncAnimation(fig, update, range(NumOfStep))

    plt.show()
