from scitools.std import *
def f(x, y):
    return peaks(x, y)

x = seq(-2, 2, 0.1)      # -2 to 2 with steps of 0.1
xv, yv = meshgrid(x, x)  # define a 2D grid with points (xv,yv)
values = f(xv, yv)       # function values
surfc(xv, yv, values,
      shading='interp',
      clevels=15,
      clabels='on',
      hidden='on',
      show=True)
