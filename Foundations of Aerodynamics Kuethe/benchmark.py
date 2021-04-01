from airfoil import *
from utils import timeit
import time
import os

XB = np.array([1.,.933,.750,.500,.250,.067,.0,.067,.25,.500,.750,.933, 1.0])
YB = np.array([.0,-.005,-.017,-.033,-.042,-.033,.0,.045,.076,.072,.044,.013,0.])

@timeit
def timedPanelPY():
    panelMethod(XB,YB,angle=8.0)
@timeit
def timedPanelf():
    os.startfile("airfoil.exe")

timedPanelPY()
timedPanelf()