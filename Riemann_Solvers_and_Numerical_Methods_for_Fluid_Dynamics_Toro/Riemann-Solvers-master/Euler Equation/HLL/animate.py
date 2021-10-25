import os
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

if __name__ == '__main__':
    while True:
        ok = False
        f = ''
        while not ok:
            f = input("Enter filename(\'#\' to exit):")
            if os.path.exists(f):
                ok = True
            else:
                if f is '#':
                    exit(0)

        fin = open(f, 'r')
        NumOfStep, NumOfPnt = fin.readline().strip().split()
        NumOfStep = int(NumOfStep)
        NumOfPnt = int(NumOfPnt)

        x = np.zeros(NumOfPnt)
        for k in range(NumOfPnt):
            x[k] = float(fin.readline().strip())

        animation_data = np.zeros((NumOfStep, NumOfPnt, 10))
        for n in range(NumOfStep):
            fin.readline()
            for k in range(NumOfPnt):
                animation_data[n][k] = fin.readline().strip().split()

        fin.close()

        fig = plt.figure()
        fig.tight_layout()
        ax1 = fig.add_subplot(511)
        ax1.set_ylabel(r'$\rho$')

        ax2 = fig.add_subplot(512)
        ax2.set_ylabel(r'$u$')

        ax3 = fig.add_subplot(513)
        ax3.set_xlabel('X')
        ax3.set_ylabel(r'$P$')

        ax4 = fig.add_subplot(514)
        ax4.set_xlabel('X')
        ax4.set_ylabel(r'$e$')

        ax5 = fig.add_subplot(515)
        ax5.set_xlabel('X')
        ax5.set_ylabel(r'$Ma$')

        line11, = ax1.plot(x, animation_data[0, :, 0], 'ro', fillstyle='none')
        line12, = ax1.plot(x, animation_data[0, :, 5])
        line21, = ax2.plot(x, animation_data[0, :, 1], 'ro', fillstyle='none')
        line22, = ax2.plot(x, animation_data[0, :, 6])
        line31, = ax3.plot(x, animation_data[0, :, 2], 'ro', fillstyle='none')
        line32, = ax3.plot(x, animation_data[0, :, 7])
        line41, = ax4.plot(x, animation_data[0, :, 3], 'ro', fillstyle='none')
        line42, = ax4.plot(x, animation_data[0, :, 8])
        line51, = ax5.plot(x, animation_data[0, :, 4], 'ro', fillstyle='none')
        line52, = ax5.plot(x, animation_data[0, :, 9])


        margin = 0.05


        def update(data):
            # rho plot
            bot = min(np.min(data[:, 0]), np.min(data[:, 5]))
            top = max(np.max(data[:, 0]), np.max(data[:, 5]))
            height = top - bot
            mh = margin * height
            if top > bot:
                ax1.set_ylim(bot - mh, top + mh)
            line11.set_ydata(data[:, 0])
            line12.set_ydata(data[:, 5])

            # u plot
            bot = min(np.min(data[:, 1]), np.min(data[:, 6]))
            top = max(np.max(data[:, 1]), np.max(data[:, 6]))
            height = top - bot
            mh = margin * height
            if top > bot:
                ax2.set_ylim(bot - mh, top + mh)
            line21.set_ydata(data[:, 1])
            line22.set_ydata(data[:, 6])

            # P plot
            bot = min(np.min(data[:, 2]), np.min(data[:, 7]))
            top = max(np.max(data[:, 2]), np.max(data[:, 7]))
            height = top - bot
            mh = margin * height
            if top > bot:
                ax3.set_ylim(bot - mh, top + mh)
            line31.set_ydata(data[:, 2])
            line32.set_ydata(data[:, 7])

            # e plot
            bot = min(np.min(data[:, 3]), np.min(data[:, 8]))
            top = max(np.max(data[:, 3]), np.max(data[:, 8]))
            height = top - bot
            mh = margin * height
            if top > bot:
                ax4.set_ylim(bot - mh, top + mh)
            line41.set_ydata(data[:, 3])
            line42.set_ydata(data[:, 8])

            # Ma plot
            bot = min(np.min(data[:, 4]), np.min(data[:, 9]))
            top = max(np.max(data[:, 4]), np.max(data[:, 9]))
            height = top - bot
            mh = margin * height
            if top > bot:
                ax5.set_ylim(bot - mh, top + mh)
            line51.set_ydata(data[:, 4])
            line52.set_ydata(data[:, 9])


        ret = animation.FuncAnimation(fig, update, animation_data)

        plt.tight_layout()
        plt.show()
