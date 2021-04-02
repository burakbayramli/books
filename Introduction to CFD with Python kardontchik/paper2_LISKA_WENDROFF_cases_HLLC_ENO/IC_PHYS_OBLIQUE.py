"""
   file: IC_PHYS_OBLIQUE
   imposes the IC on the cells of the physical space rotated by
   any angle between [-89,+89] degrees
"""

import numpy as np
from numpy import pi as pi

def ic_phys_2D_angle(upper_left,lower_left, upper_right,lower_right,x,y,x0,y0,angle):
    """
    Imposes the IC on the cells of the physical space.
    p: array with the coordinates of the center of the cells
    (x0,y0): (x,y)-coordinate where the central discontinuity is at t = 0
    rotation angle in degrees (counterclockwise is positive)
    """
    angle = angle*(pi/180.0)
    cc1 = np.tan(angle)
    cc2 = np.tan(angle - pi/2)
    
    pp = - np.ones((len(x),len(y)))
    dd = - np.ones((len(x),len(y)))
    uu = 99.0*np.ones((len(x),len(y)))
    vv = 99.0*np.ones((len(x),len(y)))
    
    # quadrants rotated around x = x0, y = y0 by angle
    i0 = np.argmin(np.abs(x-x0))
    j0 = np.argmin(np.abs(y-y0))

    if angle >= 0:
        for i in range (len(x)):
            for j in range (len(y)):
                if j <= j0 + cc1*(i - i0) and j < j0 + cc2*(i - i0):
                    # lower left quadrant
                    pp[i,j] = lower_left[0]
                    dd[i,j] = lower_left[1]
                    uu[i,j] = np.cos(angle)*lower_left[2] - np.sin(angle)*lower_left[3]
                    vv[i,j] = np.sin(angle)*lower_left[2] + np.cos(angle)*lower_left[3]
                elif j < j0 + cc1*(i - i0) and j >= j0 + cc2*(i - i0):
                    # lower right quadrant
                    pp[i,j] = lower_right[0]
                    dd[i,j] = lower_right[1]
                    uu[i,j] = np.cos(angle)*lower_right[2] - np.sin(angle)*lower_right[3]
                    vv[i,j] = np.sin(angle)*lower_right[2] + np.cos(angle)*lower_right[3]
                elif j >= j0 + cc1*(i - i0) and j > j0 + cc2*(i - i0):
                    # upper right quadrant
                    pp[i,j] = upper_right[0]
                    dd[i,j] = upper_right[1]
                    uu[i,j] = np.cos(angle)*upper_right[2] - np.sin(angle)*upper_right[3]
                    vv[i,j] = np.sin(angle)*upper_right[2] + np.cos(angle)*upper_right[3]
                else:
                    # upper left quadrant
                    pp[i,j] = upper_left[0]
                    dd[i,j] = upper_left[1]
                    uu[i,j] = np.cos(angle)*upper_left[2] - np.sin(angle)*upper_left[3]
                    vv[i,j] = np.sin(angle)*upper_left[2] + np.cos(angle)*upper_left[3]
    else:
        for i in range (len(x)):
            for j in range (len(y)):
                if j <= j0 + cc1*(i - i0) and j > j0 + cc2*(i - i0):
                    # lower left quadrant
                    pp[i,j] = lower_left[0]
                    dd[i,j] = lower_left[1]
                    uu[i,j] = np.cos(angle)*lower_left[2] - np.sin(angle)*lower_left[3]
                    vv[i,j] = np.sin(angle)*lower_left[2] + np.cos(angle)*lower_left[3]
                elif j < j0 + cc1*(i - i0) and j <= j0 + cc2*(i - i0):
                    # lower right quadrant
                    pp[i,j] = lower_right[0]
                    dd[i,j] = lower_right[1]
                    uu[i,j] = np.cos(angle)*lower_right[2] - np.sin(angle)*lower_right[3]
                    vv[i,j] = np.sin(angle)*lower_right[2] + np.cos(angle)*lower_right[3]
                elif j >= j0 + cc1*(i - i0) and j < j0 + cc2*(i - i0):
                    # upper right quadrant
                    pp[i,j] = upper_right[0]
                    dd[i,j] = upper_right[1]
                    uu[i,j] = np.cos(angle)*upper_right[2] - np.sin(angle)*upper_right[3]
                    vv[i,j] = np.sin(angle)*upper_right[2] + np.cos(angle)*upper_right[3]
                else:
                    # upper left quadrant
                    pp[i,j] = upper_left[0]
                    dd[i,j] = upper_left[1]
                    uu[i,j] = np.cos(angle)*upper_left[2] - np.sin(angle)*upper_left[3]
                    vv[i,j] = np.sin(angle)*upper_left[2] + np.cos(angle)*upper_left[3]

    return pp,dd,uu,vv    
