"""
    VISUAL_1D
    generates plots for density, velocity, pressure and energy (d,u,p,e)
"""

import numpy as np
import matplotlib.pylab as plt

class VISUAL:
    def __init__(self,test_num,CFL,x_cell,x01,d01,u01,p01,e01, \
                 dd,uu,pp,ee,interp=1,Igodu=0):
        self.test_num,self.CFL,self.x_cell = test_num,CFL,x_cell
        self.x01,self.d01,self.u01,self.p01,self.e01 = x01,d01,u01,p01,e01
        self.dd,self.uu,self.pp,self.ee = dd,uu,pp,ee
        self.interp = interp
        self.Igodu = Igodu
    def __call__(self):
        test_num,CFL,x_cell = self.test_num,self.CFL,self.x_cell
        x01,d01,u01,p01,e01 = self.x01,self.d01,self.u01,self.p01,self.e01
        dd,uu,pp,ee = self.dd,self.uu,self.pp,self.ee
        interp = self.interp
        Igodu = self.Igodu
        
        plt.figure()
        plt.subplot(2,2,1)
        plt.plot(x01,d01,'b',x_cell,dd,'r')
        plt.grid()
        plt.title('test # %d, cfl = %g' % (test_num,CFL))
        plt.ylabel('density')
        plt.subplot(2,2,2)
        plt.plot(x01,u01,'b',x_cell,uu,'r')
        plt.grid()
        if interp == 0:
            if Igodu == 0:
                plt.title('exact=blue,hllc=red')
            if Igodu == 1:
                plt.title('exact=blue,godunov=red')
        if interp == 1:
            if Igodu == 0:
                plt.title('exact=blue,hllc+eno=red')
            if Igodu == 1:
                plt.title('EXACT=blue,godunov+eno=red')
        ax = plt.subplot(2,2,2)
        ax.yaxis.set_label_position('right')
        plt.ylabel('speed u')
        plt.subplot(2,2,3)
        plt.plot(x01,p01,'b',x_cell,pp,'r')
        plt.grid()
        plt.ylabel('pressure')
        plt.xlabel('x')
        plt.subplot(2,2,4)
        plt.plot(x01,e01,'b',x_cell,ee,'r')
        plt.grid()
        ax = plt.subplot(2,2,4)
        ax.yaxis.set_label_position('right')
        plt.ylabel('internal energy')
        plt.xlabel('x')
        plt.show()
