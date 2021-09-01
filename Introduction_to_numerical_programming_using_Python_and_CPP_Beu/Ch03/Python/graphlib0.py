#------------------------------- graphlib0.py -------------------------------
# Graphics library based on Tkinter
#----------------------------------------------------------------------------
from math import *
from tkinter import *

root = Tk()                                           # create Tk root widget
root.title("graphlib0 v1.0")

#============================================================================
def MainLoop():                                       # creates Tk event loop
   root.mainloop()

#============================================================================
def GraphInit(nxwin, nywin):       # creates Canvas widget and returns object
   global w, nxw, nyw                          # make canvas object available

   nxw = nxwin; nyw = nywin                                     # canvas size
   w = Canvas(root, width=nxw, height=nyw, bg = "white")      # create canvas
   w.pack()                                             # make canvas visible
   return w

def Nint(x): return int(floor(x + 0.5))                     # nearest integer

#============================================================================
def Plot0(x, y, n, col, fxmin, fxmax, fymin, fymax, xtext, ytext, title):
#----------------------------------------------------------------------------
#  Plots a real function of one variable specified by a set of (x,y) points.
#
#  x[]   - abscissas of tabulation points (x[1] through x[n])
#  y[]   - ordinates of tabulation points (y[1] through y[n])
#  n     - number of tabulation points
#  col   - plot color ("red", "green", "blue" etc.)
#  fxmin - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
#  fxmax - max fractional x-limit of viewport
#  fymin - min fractional y-limit of viewport (0 < fymin < fymax < 1)
#  fymax - max fractional y-limit of viewport
#  xtext - x-axis title
#  ytext - y-axis title
#  title - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size

   xmin = min(x[1:n+1]); xmax = max(x[1:n+1])            # user domain limits
   ymin = min(y[1:n+1]); ymax = max(y[1:n+1])
                                           # corrections for horizontal plots
   if (ymin == 0.0 and ymax == 0.0): ymin = -1e0; ymax = 1e0
   if (ymin == ymax): ymin *= 0.9; ymax *= 1.1

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)     # canvas domain
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)
   w.create_rectangle(ixmin,iymax,ixmax,iymin)              # draw plot frame

   w.create_text((ixmin+ixmax)/2,iymin+10,text=xtext,anchor=N) # x-axis title
   w.create_text(ixmin-10,(iymin+iymax)/2,text=ytext,anchor=E) # y-axis title
   w.create_text((ixmin+ixmax)/2,iymax-10,text=title,anchor=S)   # plot title
                                                           # labels axes ends
   w.create_text(ixmin,iymin+10,text="{0:5.2f}".format(xmin),anchor=NW)
   w.create_text(ixmax,iymin+10,text="{0:5.2f}".format(xmax),anchor=NE)
   w.create_text(ixmin-10,iymin,text="{0:5.2f}".format(ymin),anchor=E)
   w.create_text(ixmin-10,iymax,text="{0:5.2f}".format(ymax),anchor=E)

   ax = (ixmax-ixmin)/(xmax-xmin)               # x-axis scaling coefficients
   bx = ixmin - ax*xmin
   ay = (iymax-iymin)/(ymax-ymin)               # y-axis scaling coefficients
   by = iymin - ay*ymin
                                                                  # draw axes
   if (xmin*xmax < 0): w.create_line(Nint(bx),iymin,Nint(bx),iymax)  # y-axis
   if (ymin*ymax < 0): w.create_line(ixmin,Nint(by),ixmax,Nint(by))  # x-axis

   ix0 = Nint(ax*x[1]+bx); iy0 = Nint(ay*y[1]+by)                 # 1st point
   for i in range(2,n+1):
      ix = Nint(ax*x[i]+bx); iy = Nint(ay*y[i]+by)                # new point
      w.create_line(ix0,iy0,ix,iy,fill=col)                       # draw line
      ix0 = ix; iy0 = iy                                         # save point
