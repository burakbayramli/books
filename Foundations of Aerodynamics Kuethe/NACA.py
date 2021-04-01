# Python 3
# X,Y sactters defining NACA series airfoil profiles
# Used to generate meshes for airfoils.

import numpy as np
import math

def fourDigitSeries(digits:int, numPanels: int):
    """
    Simple functions which returnsNACA profiles as needed by aifoil.py

    returns  X coords, Y coords -> Both numpy arrays of shape (numPanels+1)
    The format is specifically for the vortex panel method.
    return X coords and Y coords describing a NACA foil. Equations are taken from Moran (2003) 

    http://web.stanford.edu/~cantwell/AA200_Course_Material/The%20NACA%20airfoil%20series.pdf
        > " The first digit specifies the maximum camber (m) in percentage of the chord (airfoil length),
        the second indicates the position of the maximum camber (p) in tenths of chord, and the last two
        numbers provide the maximum thickness (t) of the airfoil in percentage of chord. For example, the
        NACA 2415 airfoil has a maximum thickness of 15% with a camber of 2% located 40% back from
        the airfoil leading edge (or 0.4c)"

    Author: Tristan C.B.

    references: 
        Moran, Jack. An Introduction to Theoretical and Computational Aerodynamics. Dover Publications, 2003.
        M. and Chowc-Y. Foundations of Aerodynamics: Bases of Aerodynamic Design — Fourth Edition. John Wieley & Sons, 1968.
    # """
    # Panels should be a multiple a 2.
    assert numPanels % 2 == 0

    digits = str(digits).zfill(4)
    
    # Ensures proper input
    # if len(digits) > 4:
    #     raise InputError("Invalid input... 4 digits series?")
    # elif len(digits) < 4:
    #     while len(digits) < 4:
    #         digits = '0'+digits

    ## Definitions ##
    # maxCamber = location of the max camber
    # cpMaxCamber = chordwise position of the mmaximum camber
    # thicknessRatio =  airfoil having a thickness XX % (base chord) of the chord.
    # c = chord length = 1

    ## 
    # Extracts digits describing specific physical params.
    maxCamber, cpMaxCamber, thicknessRatio = [int(i) for i in [digits[0], digits[1:2], digits[2:4]]]
    # Convert to format needed for computaion
    epsilon = maxCamber/100
    p = cpMaxCamber/10
    t = thicknessRatio/100
    c = 1

    #
    length = int(numPanels/2+1)
    # Cosine spacing for discretization scheme applied to half circle b/c we are mirroring it, we want y_upper and y_lowwer for same x
    x = np.asarray(([0.5*(np.cos(i)+1) for i in np.linspace(0,np.pi,num=length)]))

    # Spacing increasing resolution near begining of profile.
    # x = np.linspace(1,0,int(numPanels/2+1))**2 # power of 2 will increase resoltion near the leading edge

    meanCamberLine = np.zeros((x.shape))
    yt, dycOVERdx, x_upper, x_lower, y_upper, y_lower, theta  = [np.zeros((x.shape)) for i in range(7)]

    # Main loop to derive coords from equation.
    for i, ij in enumerate(x):
        if i >= 0 and ij <= p:
            meanCamberLine[i] =  epsilon/p**2 * (2*p*ij-ij**2)
            dycOVERdx[i] = 2 * epsilon/p**2 * (p - ij/c)
        else:
            meanCamberLine[i] = epsilon/(1-p)**2 * ((1-2*p)+2*p*ij-ij**2)
            dycOVERdx[i] = 2 * epsilon / (1-p)**2 * (p - ij/c)

        yt[i] = t/0.2*(0.2969*np.sqrt(ij) - 0.1260*ij - 0.3516*ij**2 + 0.2843*ij**3 - 0.1015*ij**4)
        theta[i] = np.arctan(dycOVERdx[i])

        x_upper[i], y_upper[i] = (ij - yt[i]*np.sin(theta[i])), (meanCamberLine[i] + yt[i]*np.cos(theta[i]))
        x_lower[i], y_lower[i] = (ij + yt[i]*np.sin(theta[i])), (meanCamberLine[i] - yt[i]*np.cos(theta[i]))
    
    # Stitch the arrays together to get convention clockwise from leading edge
    # Takes the x_lower array in inverse order and removes coinciding point (0,0)
    XB = np.hstack((x_lower,x_upper[::-1][1:]))
    YB = np.hstack((y_lower,y_upper[::-1][1:]))
    
    # To run the airfoil simulation we need the same format at prescribed bellow.
    # The bellow array of for a NACA 2412, with slight modifications to make the trailing edge close
    YB[0], YB[-1] = 0, 0
    # YB[int(YB.shape[0]/2)] = 0

    # print(
    # f'''
    # ## NACA foil 4 digit series: {digits} ##
    # Physical params:
    # epsilon => {epsilon} 
    # p       => {p}
    # t       => {t}
    # ########################################
    # --- XB{XB.shape} ---
    #         {XB}
    # --- YB{YB.shape} ---
    #         {YB}
    # '''
    # )

    return XB, YB

def ansysPtFormat(outputPath, XB, YB):
    """
    outputPath: String
    XB: np.array
    YB: np.array (with shape corresponding to XB)
    Writes to a file the X and Y points formatted for input as coord file
    """
    # Linear transformations to get the left most X value (and corresponding Y val) 
    # clipped to 0,0
    middleXB = int(math.ceil(XB.shape[0]/2))
    tipXval     = np.amin(XB)
    tipIndexVal = np.where(XB == np.amin(tipXval))
    tipYval = YB[tipIndexVal]
    # Translate
    XB = XB - tipXval
    YB = YB - tipYval
    with open(outputPath, 'w') as file:
        for i, ij in enumerate(XB):
            file.writelines(f"1 {i+1} {ij} {YB[i]} {0}\n")

if __name__ == "__main__":
    ## Unit test ## 
    # Points for NACA 2412 from two independent sources.
    XB2412 = np.array([1.,.933,.750,.500,.250,.067,.0,.067,.25,.500,.750,.933, 1.0])
    YB2412 = np.array([.0,-.005,-.017,-.033,-.042,-.033,.0,.045,.076,.072,.044,.013,0.])
    # The specific format as a string is as so fortran knows they are all real numbers.

    XB2412_foilTools = np.array([1.,.95,.90,.8,.7,.6,.5,.4,.3,.25,.20,0.15,0.1,0.075,0.0500,0.025,0.0125,0.0,0.0125,0.0250,0.05,0.075,0.1,0.15,0.20,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,1.0])
    YB2412_foilTools = np.array([0.0013,0.0114,0.0208,0.0375,0.0518,0.0636,0.0724,0.0780,0.0788,0.0767,0.0726,0.0661,0.0563,0.0496,0.0413,0.0299,0.0215,0.0000,-0.0165,-0.0227,-0.0301,-0.0346,-0.0375,-0.0410,-0.0423,-0.0422,-0.0412,-0.0380,-0.0334,-0.0276,-0.0214,-0.0150,-0.0082,-0.0048,-0.0013])
    
    # Chose to conduct unit test on data from website, Book had slightly altered coords to have the first point start at (0,0)
    # XB, YB = fourDigitSeries(2412, len(YB2412_foilTools)-1, plot=True)
    # assert XB2412_foilTools.all() == XB.all()
    # assert YB2412_foilTools.all() == YB.all()
    # PASS

    # USER DEFINED VARIABLES
    AIRFOIL_NACA_SERIES_NUMBER      = 2412
    strAIRFOIL_NACA_SERIES_NUMBER   = str(AIRFOIL_NACA_SERIES_NUMBER).zfill(4)

    NUMBER_OF_PANELS                = 512

    # Resquest X,Y scatter defining airfoil profile
    XB, YB = fourDigitSeries(2412, NUMBER_OF_PANELS)

    # Write values to text with tip of profile starting at 0,0
    ansysPtFormat("./NACA2412.txt", XB, YB)

    import matplotlib.pyplot as plt


    fig, ax = plt.subplots()
    ax.plot(XB, YB, 'o', color='black', alpha=0.2)
    ax.plot(XB2412, YB2412, '_', color='red', alpha=0.2)
    ax.plot(XB2412_foilTools, YB2412_foilTools, 'x', color='purple', alpha=0.2)
    plt.show()
    plt.clf()

    # from utils import rotate_around_point
    # You should not rotate mesh when modelling just change your frame of reference but anyways, left this here.
    # XB, YB = rotate_around_point((XB,YB),math.radians(8),org=(0.4,0))

    