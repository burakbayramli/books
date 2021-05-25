"""
   file: IC_PHYS
   imposes the IC on the cells of the physical space
"""

import numpy as np

def ic_phys_1D(left,right,p,x0=0.5):
    """
    Imposes the IC on the cells of the physical space.
    left = [pL,dL,uL,vL]
    right = [pR,dR,uR,vR]
    p: array with the coordinates of the center of the cells
    x0: x-coordinate where the initial discontinuity is at t = 0
    """
    pp = - np.ones(len(p))
    dd = - np.ones(len(p))
    uu = 99.0*np.ones(len(p))
    vv = 99.0*np.ones(len(p))
    
    # initial discontinuity at x = x0 
    indL = np.where(p < x0)
    indR = np.where(p >= x0)

    pp[indL] = left[0]
    dd[indL] = left[1]
    uu[indL] = left[2]
    vv[indL] = left[3]

    pp[indR] = right[0]
    dd[indR] = right[1]
    uu[indR] = right[2]
    vv[indR] = right[3]

    return pp,dd,uu,vv

def ic_phys_2D(upper_left,lower_left, upper_right,lower_right,x,y,x0=0.5,y0=0.5):
    """
    Imposes the IC on the cells of the physical space.
    p: array with the coordinates of the center of the cells
    x0: x-coordinate where the initial discontinuity is at t = 0
    """
    pp = - np.ones((len(x),len(y)))
    dd = - np.ones((len(x),len(y)))
    uu = 99.0*np.ones((len(x),len(y)))
    vv = 99.0*np.ones((len(x),len(y)))
    
    # initial discontinuity at x = x0, y = y0

    xind_left = np.where(x < x0)
    xind_right = np.where(x >= x0)
    yind_lower = np.where(y < y0)
    yind_upper = np.where(y >= y0)

    # np.ones((# of rows, # of columns))
    pp_lower_left = lower_left[0]*np.ones((np.size(xind_left),np.size(yind_lower)))
    dd_lower_left = lower_left[1]*np.ones((np.size(xind_left),np.size(yind_lower)))
    uu_lower_left = lower_left[2]*np.ones((np.size(xind_left),np.size(yind_lower)))
    vv_lower_left = lower_left[3]*np.ones((np.size(xind_left),np.size(yind_lower)))
    pp_lower_right = lower_right[0]*np.ones((np.size(xind_right),np.size(yind_lower)))
    dd_lower_right = lower_right[1]*np.ones((np.size(xind_right),np.size(yind_lower)))
    uu_lower_right = lower_right[2]*np.ones((np.size(xind_right),np.size(yind_lower)))
    vv_lower_right = lower_right[3]*np.ones((np.size(xind_right),np.size(yind_lower)))
    pp_upper_left = upper_left[0]*np.ones((np.size(xind_left),np.size(yind_upper)))
    dd_upper_left = upper_left[1]*np.ones((np.size(xind_left),np.size(yind_upper)))
    uu_upper_left = upper_left[2]*np.ones((np.size(xind_left),np.size(yind_upper)))
    vv_upper_left = upper_left[3]*np.ones((np.size(xind_left),np.size(yind_upper)))
    pp_upper_right = upper_right[0]*np.ones((np.size(xind_right),np.size(yind_upper)))
    dd_upper_right = upper_right[1]*np.ones((np.size(xind_right),np.size(yind_upper)))
    uu_upper_right = upper_right[2]*np.ones((np.size(xind_right),np.size(yind_upper)))
    vv_upper_right = upper_right[3]*np.ones((np.size(xind_right),np.size(yind_upper)))

    pp_lower = np.concatenate((pp_lower_left,pp_lower_right),axis=0)
    pp_upper = np.concatenate((pp_upper_left,pp_upper_right),axis=0)
    pp = np.concatenate((pp_lower,pp_upper),axis=1)

    dd_lower = np.concatenate((dd_lower_left,dd_lower_right),axis=0)
    dd_upper = np.concatenate((dd_upper_left,dd_upper_right),axis=0)
    dd = np.concatenate((dd_lower,dd_upper),axis=1)

    uu_lower = np.concatenate((uu_lower_left,uu_lower_right),axis=0)
    uu_upper = np.concatenate((uu_upper_left,uu_upper_right),axis=0)
    uu = np.concatenate((uu_lower,uu_upper),axis=1)

    vv_lower = np.concatenate((vv_lower_left,vv_lower_right),axis=0)
    vv_upper = np.concatenate((vv_upper_left,vv_upper_right),axis=0)
    vv = np.concatenate((vv_lower,vv_upper),axis=1)
    
    return pp,dd,uu,vv    
