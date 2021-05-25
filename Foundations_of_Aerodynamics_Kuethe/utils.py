# Python 3
# Utils for airfoil.py
# Author: Tristan CB
import numpy as np
import time
import imageio
import matplotlib.pyplot as plt
import matplotlib.lines as lines
from matplotlib import gridspec
from matplotlib import pyplot, transforms
import os
import math
import NACA
from airfoil import panelMethod

def plotFoil(seriesNumber, angle, panels = [12,48]):
        fig, ax = plt.subplots()
        gs = gridspec.GridSpec(2, 1, height_ratios=[1, 1]) 
        ax = plt.subplot(gs[0])
        # first of all, the base transformation of the data points is needed

        # Coincides closely enough with the values using the book.
        for i in panels:
            numberOfPanels = i
            XB, YB = NACA.fourDigitSeries(seriesNumber, numberOfPanels)
            results = panelMethod(XB,YB, angle=angle)
            if i == 12:
                marker  = 'o'
                color   = 'black'
                label   = f'{i}-panel solution'
            else: 
                marker  = 'x'
                color   = 'gray'
                label   = f'{i}-panel solution'

            ax.plot(results["X"], results["CP"], marker,label=label, color='black', markersize=4)

        # Format plot
        ax.set_ylim(1, -6)
        ax.set_xlim(0, 1)
        ax.grid(True)
        line = lines.Line2D([1, 0], [0,0],
                            lw=2, color='black', axes=ax)
        ax.add_line(line)
        ax.set_ylabel('Cp')
        ax.set_xlabel('x/c')
        ax.legend()
        # Plot foil under graph for cp
        ax = plt.subplot(gs[1])
        XB, YB = NACA.fourDigitSeries(seriesNumber, 48)

        # Rotate profile to match incident angle
        # We rotate profile arounf 0.4 rougly the center of mass for symmetrical NACA foil.
        for i, ij in enumerate(XB):
            XB[i], YB[i] = rotate_around_point((XB[i],YB[i]), math.radians(angle),  org=(0.4,0.0))

        ax.plot(XB, YB, marker, color='black',linestyle='solid', markersize=1)

        # Limits for foil plot
        ax.set_ylim(-0.2, 0.2)
        ax.set_xlim(0, 1)


        seriesNumbFormatted = str(seriesNumber).zfill(4)
        ax.set_xlabel(f"NACA {seriesNumbFormatted} foil -- @ {angle:.3f} deg")
        
        # Misc figure formatting
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.spines['bottom'].set_visible(False)
        plt.tick_params(bottom=False)
        ax.set_xticklabels([])
        fig.tight_layout(pad=2.0)

        # Show plot then return resutls.
        return plt

def timeit(method):
    """
    Used to time the execution of functions. Useful to use as decorator.
    """
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        print(f"{method.__name__} Took --> {(te - ts)} <-- seconds")
        return result
    return timed

def rotateNumpy(xy, radians):
    """
    Lyle Scott, III  // lyle@ls3.io
    Use numpy to build a rotation matrix and take the dot product.
    https://gist.github.com/LyleScott/e36e08bfb23b1f87af68c9051f985302
    """
    x, y = xy
    c, s = np.cos(radians), np.sin(radians)
    j = np.matrix([[c, s], [-s, c]])
    m = np.dot(j, [x, y])

    return float(m.T[0]), float(m.T[1])

def rotate_around_point(point, radians, org=(0, 0)):
    """Rotate a point around a given point."""
    x, y = point
    ox, oy = org
    qx = ox + math.cos(radians) * (x - ox) + math.sin(radians) * (y - oy)
    qy = oy + -math.sin(radians) * (x - ox) + math.cos(radians) * (y - oy)

    return qx, qy

def generateGIF(inputPATH, outputNAME):
    """
    Example use: generateGIF("./TMP_GIF/","output")
    """
    with imageio.get_writer(f'{inputPATH}{outputNAME}.gif', mode='I') as writer:
        for filename in [i for i in (os.listdir(inputPATH)) if "png" in i.split(".")[-1]]:
            pathToim = os.path.join(inputPATH,filename)
            print(pathToim)
            image = imageio.imread(pathToim)
            writer.append_data(image)

if __name__ == "__main__":

    # Generates Gif for NACA 2412 at varying angles from 0 to 15 deg
    for i, ij in enumerate(np.linspace(0,15,100)):
        seriesNumber = 2412
        seriesNumberFormatted = str(seriesNumber).zfill(4)
        plotFoil(seriesNumber,ij).savefig(f"./TMP_GIF/{str(i).zfill(5)}NACA{seriesNumberFormatted}foil.png")
    generateGIF("./TMP_GIF/","output")

## Code Graveyard ##

# For all series numbers
# for i in range(6310,9999,10):
#     print(i)
#     # Skip invalid profiles
#     if i % 100 == 0:
#         continue
#     invalidProfiles = []
#     try:
#         savFIG(i)
#     except:
#         invalidProfiles.append(i)
#         print(f"Invalid profile: {

# For all series numbers
# for i in range(6310,9999,10):
#     print(i)
#     # Skip invalid profiles
#     if i % 100 == 0:
#         continue
#     invalidProfiles = []
#     try:
#         savFIG(i)
#     except:
#         invalidProfiles.append(i)
#         print(f"Invalid profile: {i}")
# For various angles

## Plot pressure distribution for all angles in between -15 and 15


# plt.savefig(f"./TMP_GIF/{seriesNumbFormatted}-{int(angle*10)}NACAfoil.png")