"""
   IC_conditions

   Note: the function "ic_phys" for Liska-Wendroff tests and other tests
   must be defined for each test separatelay
"""
import numpy as np
from numpy import pi as pi

def find_wedge_states(p_Right,d_Right,u_Right,v_Right,MS):
    # Used in Toro explosion problem: Given the initial state to the Right
    # of the shock front and given the speed of the shock, find the
    # initial state to the Left of the shock
    # returns the Left and Right states to be used by the solver as
    # initial conditions
    
    gamma = 1.4
    # initial position of the shock, defining the Left and Right regions
    x0 = 4.0
    # Right region
    MR = 0 # Mach number
    p_Right = 1.01325e5     # 1 atm
    d_Right = 1.225         # kg/m^3
    u_Right = 0

    a_Right = np.sqrt(gamma*p_Right/d_Right)
    MR = u_Right/a_Right    # Mach # in Right region 

    # Right state
    Right = [p_Right,d_Right,u_Right,0.0]

    # Shock Mach number MS
    # Mach # of the shock as seen by the stationary air in the Right region
    MS = 1.7 # Eq (3.52)
    # Do not confuse with Mach # of the fluid, u_Right/a_Right or
    # u_Left/a_Left).We are talking here about the speed of a WAVE front,
    # not the speed of the fluid

    # Shock speed S (with respect to the stationary air in the Right region)
    S = a_Right*MS  # shock speed, Toro, Eq (3.52)


    # Left region
    # pressure and density Eq (3.54) and (3.53)

    p_Left = p_Right*(2.0*gamma*(MR - MS)**2 - (gamma - 1.0))/(gamma + 1.0)
    d_Left = d_Right*(gamma + 1.0)*(MR - MS)**2/((gamma - 1.0)*(MR-MS)**2 + 2.0)

    u_Left = (1.0 - (d_Right/d_Left))*S + u_Right*(d_Right/d_Left)  # Eq (3.56)

    a_Left = np.sqrt(gamma*p_Left/d_Left)
    ML = u_Left/a_Left

    # p_Left = 3.205 atm (324746.625 Pascal)
    # d_Left = 2.692 kg/m^3
    # u_Left = 315.27 m/s;  a_Left = 410.94 m/s. Hence:
    # ML = 0.77
    # Left state
    Left = [p_Left, d_Left,u_Left,0.0]

    # S = 578.5 m/sec  (shock wave front speed)
    # At this shock speed, with no wedge it would take about 0.035 sec to
    # traverse the complete physical domain (about 20 meters). This sets
    # the simulation time
    return Left,Right

def ic_phys_wedge(Left,Right,x0,p,tri):
    """
    Imposes the IC on the cells of the physical space for Toro wedge tests'
    """
    pp = - np.ones(len(tri))
    dd = - np.ones(len(tri))
    uu = 99.0*np.ones(len(tri))
    vv = 99.0*np.ones(len(tri))

    # Find for each triangle in 'tri' whether it is in the Left or Right
    # regions. Set, accordingly, the IC
    pmid = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])

    ind_Left = np.where(pmid[:,0] < x0)
    ind_Right = np.where(pmid[:,0] >= x0)

    pp[ind_Left] = Left[0]
    dd[ind_Left] = Left[1]
    uu[ind_Left] = Left[2]
    vv[ind_Left] = Left[3]    
    pp[ind_Right] = Right[0]
    dd[ind_Right] = Right[1]
    uu[ind_Right] = Right[2]
    vv[ind_Right] = Right[3]
    
    return pp,dd,uu,vv

def ic_phys_Left_Right(Left,Right,x0,p,tri):
    """
    Imposes the IC on the cells of the physical space assuming there is
    a Left and Right initial regions with a physical discontinuity at x0'
    """
    pp = - np.ones(len(tri))
    dd = - np.ones(len(tri))
    uu = 99.0*np.ones(len(tri))
    vv = 99.0*np.ones(len(tri))

    # Find for each triangle in 'tri' whether it is in the Left or Right
    # regions. Set, accordingly, the IC
    pmid = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])

    ind_Left = np.where(pmid[:,0] < x0)
    ind_Right = np.where(pmid[:,0] >= x0)

    pp[ind_Left] = Left[0]
    dd[ind_Left] = Left[1]
    uu[ind_Left] = Left[2]
    vv[ind_Left] = Left[3]    
    pp[ind_Right] = Right[0]
    dd[ind_Right] = Right[1]
    uu[ind_Right] = Right[2]
    vv[ind_Right] = Right[3]
    
    return pp,dd,uu,vv

def ic_phys_toro_explosion(inside,outside,R,p,tri):
    """
    Imposes the IC on the cells of the physical space for Toro explosion tests
    """
    pp = - np.ones(len(tri))
    dd = - np.ones(len(tri))
    uu = 99.0*np.ones(len(tri))
    vv = 99.0*np.ones(len(tri))
    
    # Find for each triangle in 'tri' whether it is inside or outside the
    # circle of radius R
    # set, accordingly, the IC
    pmid = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])

    Rsq = R**2
    ind_in = np.where(pmid[:,0]**2 + pmid[:,1]**2 < Rsq)
    ind_out = np.where(pmid[:,0]**2 + pmid[:,1]**2 >= Rsq)
    
    pp[ind_in] = inside[0]
    dd[ind_in] = inside[1]
    uu[ind_in] = inside[2]
    vv[ind_in] = inside[3]    
    pp[ind_out] = outside[0]
    dd[ind_out] = outside[1]
    uu[ind_out] = outside[2]
    vv[ind_out] = outside[3]
    
    return pp,dd,uu,vv

def ic_phys(upper_left,lower_left,upper_right,lower_right,p,tri,x0=0.5,y0=0.5):
    """
    Imposes the IC on the cells of the physical space for Liska-Wendroff tests
    """
    pp = - np.ones(len(tri))
    dd = - np.ones(len(tri))
    uu = 99.0*np.ones(len(tri))
    vv = 99.0*np.ones(len(tri))
    
    # initial discontinuities at x = x0 and y = y0
    #x0 = 0.5
    #y0 = 0.5
    # Find to which quandrant each triangle in 'tri' belongs to and
    # set, accordingly, the IC
    pmid = (1.0/3.0)*(p[tri[:,0]] + p[tri[:,1]] + p[tri[:,2]])
    # 00 = lower Left, 01 = upper Left, 10 = lower Right, 11 = upper Right
    ind00 = np.logical_and(pmid[:,0] < x0, pmid[:,1] < y0)
    ind01 = np.logical_and(pmid[:,0] < x0, pmid[:,1] >= y0)
    ind10 = np.logical_and(pmid[:,0] >= x0, pmid[:,1] < y0)
    ind11 = np.logical_and(pmid[:,0] >= x0, pmid[:,1] >= y0)

    pp[ind00] = lower_left[0]
    dd[ind00] = lower_left[1]
    uu[ind00] = lower_left[2]
    vv[ind00] = lower_left[3]

    pp[ind01] = upper_left[0]
    dd[ind01] = upper_left[1]
    uu[ind01] = upper_left[2]
    vv[ind01] = upper_left[3]

    pp[ind10] = lower_right[0]
    dd[ind10] = lower_right[1]
    uu[ind10] = lower_right[2]
    vv[ind10] = lower_right[3]

    pp[ind11] = upper_right[0]
    dd[ind11] = upper_right[1]
    uu[ind11] = upper_right[2]
    vv[ind11] = upper_right[3]

    return pp,dd,uu,vv

