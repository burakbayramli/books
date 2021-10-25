import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

if __name__ == '__main__':
    while True:
        ok = False
        f = ''
        while not ok:
            f = input("Enter filename:")
            if os.path.exists(f):
                ok = True
            else:
                if f is '#':
                    exit(0)

        fin = open(f, 'r')
        NumOfStep, NumOfPnt = fin.readline().strip().split()
        NumOfStep = int(NumOfStep)
        NumOfPnt = int(NumOfPnt)

        x = np.array([float(c) for c in fin.readline().strip().split()])

        all_data = []
        for n in range(NumOfStep):
            cur_data = [float (c) for c in fin.readline().strip().split()]
            all_data.append(cur_data)
        animation_data = np.array(all_data)

        fin.close()

        fig = plt.figure()
        ax = fig.add_subplot(111)
        ax.set_ylabel(r'$u$')
        ax.set_xlabel('X')
        lb = os.path.splitext(f)[0]
        line, = ax.plot(x, animation_data[0, :], '^', label=lb)
        ax.legend()

        cnt = 1

        def update(data):
            global cnt

            lower = min(data)
            upper = max(data)
            gap = upper - lower
            margin = 0.05
            bottom_lim = lower - gap * margin
            top_lim = upper + gap * margin
            ax.set_ylim(bottom_lim, top_lim)

            line.set_ydata(data)
            
            ax.set_title('t = {}'.format(cnt))
            if cnt == 32:
                plt.savefig('{}.png'.format(lb))

            cnt = cnt + 1
            
            return line

        a = animation.FuncAnimation(fig, update, animation_data)

        plt.show()