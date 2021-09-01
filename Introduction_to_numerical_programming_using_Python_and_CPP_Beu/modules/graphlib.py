#------------------------------- graphlib.py --------------------------------
#  Graphics library based on Tkinter.
#  Author: Titus Beu, 2013
#----------------------------------------------------------------------------
from math import *
from utils import *
from tkinter import *

root = Tk()                                           # create Tk root widget
root.title("graphlib v1.0")

#============================================================================
def MainLoop():                                       # creates Tk event loop
   root.mainloop()

#============================================================================
def GraphUpdate():                                   # updates Tk root widget
   root.update()

#============================================================================
def GraphInit(nxwin, nywin):       # creates Canvas widget and returns object
   global w, nxw, nyw                          # make canvas object available

   nxw = nxwin; nyw = nywin                                     # canvas size
   w = Canvas(root, width=nxw, height=nyw, bg = "white")      # create canvas
   w.pack()                                             # make canvas visible
   return w

#============================================================================
def GraphClear():                          # deletes content of Canvas widget
   global w, nxw, nyw                                # canvas object and size
   w.delete(ALL)

#============================================================================
def Limits(xmin, xmax, maxint):
#----------------------------------------------------------------------------
#  Replaces the limits xmin and xmax of a real interval with the limits of
#  the smallest extended inteval which includes a number <= maxint of
#  subintervals of length d * 10**p, with d = 1, 2, 5 and p integer.
#
#  scale - scale factor (10**p)
#  nsigd - relevant number of significant digits
#  nintv - number of subintervals
#
#  Returns: xmin, xmax, scale, nsigd, nintv
#----------------------------------------------------------------------------
   eps = 1e-5                                  # relative precision criterion
   xfact = [0.5e0,0.5e0,0.4e0]                            # 0.5*0.5*0.4 = 0.1

   if (abs(xmax - xmin) < 10e0*eps*abs(xmax)):
      corrmin = 1e0 - 10e0 * Sign(eps,xmin)
      corrmax = 1e0 + 10e0 * Sign(eps,xmax)
      xmin *= corrmin
      xmax *= corrmax
                                                       # initial scale factor
   factor = 1e0/(eps * min(Magn(xmin),Magn(xmax))) if (xmin * xmax) else \
            1e0/(eps * max(Magn(xmin),Magn(xmax)))

   corrmin = 1e0 + Sign(eps,xmin)                               # corrections
   corrmax = 1e0 - Sign(eps,xmax) 
   for i in range(1,100):                              # multiply iteratively
      xmins = floor(xmin * factor * corrmin)           # factor with xfact[]
      xmaxs = ceil (xmax * factor * corrmax)           # until the no. of 
      xnint = abs(xmaxs - xmins)                       # subintervals becomes
      if (xnint <= maxint): break                      # xnint <= maxint
      modi = i % 3
      factor = factor * xfact[modi]

   factor = 1e0 / factor                                       # scale factor
   xmin = xmins * factor                                      # xmin and xmax
   xmax = xmaxs * factor 
   scale = max(Magn(xmin),Magn(xmax))                          # scale factor
   factor = max(abs(xmins),abs(xmaxs))
   for i in range(1,modi+1): factor = factor / xfact[i]
   nsigd = int(log10(factor) + 1)                 # no. of significant digits
   nintv = Nint(xnint)                                  # no. of subintervals

   return (xmin, xmax, scale, nsigd, nintv)

#============================================================================
def FormStr(x, scale, nsigd):
#----------------------------------------------------------------------------
#  Formats the number x (with factor scale) to nsigd significant digits
#  returning the mantissa in mant[] and the exponent of 10 in expn[].
#
#  Returns: mant, expn
#----------------------------------------------------------------------------
   ndigmax = 5                                        # maximum no. of digits
   mant = expn = ""

   n = Nint(log10(scale))                                          # exponent
   if ((n < -1) or (n > 3)):
      expn = repr(n)
      x = x / scale                                       # divide x by scale
      n = 0

   n += 1                                # no. of digits before decimal point
   ndig = min(ndigmax,max(nsigd,n))                     # total no. of digits
   ndec = ndig - n                                          # no. of decimals
   x = round(x,ndec)
   mant = "{0:{1:}.{2:}f}".format(x,ndig,ndec)

   return (mant, expn)

#============================================================================
def Plot(x, y, n, col, sty, fxmin, fxmax, fymin, fymax, xtext, ytext, title):
#----------------------------------------------------------------------------
#  Plots a real function of one variable specified by a set of (x,y) points.
#  The x and y-domains are extended to fit at most 10 intervals expressible
#  as d * 10^p, with d = 1, 2, 5 and p integer.
#
#  x[]   - abscissas of tabulation points (x[1] through x[n])
#  y[]   - ordinates of tabulation points (y[1] through y[n])
#  n     - number of tabulation points
#  col   - plot color ("red", "green", "blue" etc.)
#  sty   - plot style: 0 - scatter plot, 1 - line plot, 2 - polar plot,
#                      3 - drop lines, 4 - histogram
#  fxmin - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
#  fxmax - max fractional x-limit of viewport
#  fymin - min fractional y-limit of viewport (0 < fymin < fymax < 1)
#  fymax - max fractional y-limit of viewport
#  xtext - x-axis title; for "None" - axis is not labeled
#  ytext - y-axis title; for "None" - axis is not labeled
#  title - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size
   maxintx = maxinty = 10              # max. number of labeling subintervals

   xmin = min(x[1:n+1]); xmax = max(x[1:n+1])                 # domain limits
   ymin = min(y[1:n+1]); ymax = max(y[1:n+1])

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)   # viewport limits
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)

   if (sty == 2):                                                # polar plot
      xmin = min(xmin,ymin); xmax = max(xmax,ymax)       # make domain square
      xmax = max(abs(xmin),abs(xmax)); xmin = -xmax
      ymin = xmin; ymax = xmax
      if (ixmax-ixmin > iymin-iymax):                # adjust viewport limits
         c = 0.5*(ixmin+ixmax)
         d = 0.5*(iymin-iymax)
         ixmin = c - d; ixmax = c + d
      if (ixmax-ixmin < iymin-iymax):
         c = 0.5*(iymin+iymax)
         d = 0.5*(ixmax-ixmin)
         iymin = c + d; iymax = c - d

   w.create_rectangle(ixmin,iymax,ixmax,iymin)              # draw plot frame

   nfont = min(int((ixmax-ixmin)/60.) + 3,12)                     # font size
   font1 = ("Helvetica",nfont)                              # axis label font
   font2 = ("Helvetica",Nint(1.2*nfont))                    # axis title font
   font3 = ("Helvetica",Nint(1.4*nfont))                    # plot title font
   if ((ixmax-ixmin) < 3 * (iymin-iymax)):
      w.create_text((ixmin+ixmax)/2,iymax-3*nfont,text=title,font=font3)
   else:
      w.create_text(ixmax,iymax,text=title,font=font2,anchor="ne")
      maxinty = max(5,(iymin-iymax)/(ixmax-ixmin)*maxintx)
                                                                     # X-AXIS
   (xmin,xmax,scale,nsigd,nintv) = Limits(xmin,xmax,maxintx)  # extend limits
   ax = (ixmax-ixmin)/(xmax-xmin)                    # x scaling coefficients
   bx = ixmin - ax*xmin

   tic =(ixmax-ixmin)/100.                                       # tic length
   h = (xmax-xmin)/nintv; htic = ax * h                       # labeling step
   iytext = iymin + 1.5*nfont 
   for i in range(0,nintv+1):                                    # label axis
      ix = Nint(ixmin + i*htic)
      w.create_line(ix,iymin,ix,iymin-tic)                             # tics
      w.create_line(ix,iymax,ix,iymax+tic)
      if (xtext != "None"):
         (mant,expn) = FormStr(xmin+i*h,scale,nsigd)
         w.create_text(ix,iytext,text=mant,font=font1)               # labels

   if (xtext != "None"):
      if ((scale<0.1) or (scale>1000.0)): xtext = xtext + " 1e" + expn 
      ixtext = (ixmin+ixmax)/2
      iytext = iytext + 2*nfont
      w.create_text(ixtext,iytext,text=xtext,font=font2)            # x title
                                                                     # Y-AXIS
   if (ymin == 0.0 and ymax == 0.0): ymin = -1e0; ymax = 1e0     # horizontal
   if (abs(ymax-ymin) < 1e-5*abs(ymax)): ymin *= 0.9; ymax *= 1.1
   (ymin,ymax,scale,nsigd,nintv) = Limits(ymin,ymax,maxinty)  # extend limits
   ay = (iymax-iymin)/(ymax-ymin)                    # y scaling coefficients
   by = iymin - ay*ymin

   h = (ymax-ymin)/nintv; htic = ay * h                  # labeling step size
   ixtext = ixmin - nfont 
   for i in range(0,nintv+1):                                    # label axis
      iy = Nint(iymin + i*htic)
      w.create_line(ixmin,iy,ixmin+tic,iy)                             # tics
      w.create_line(ixmax,iy,ixmax-tic,iy)
      if (ytext != "None"):
         (mant,expn) = FormStr(ymin+i*h,scale,nsigd)
         w.create_text(ixtext,iy,text=mant,font=font1,anchor="e")    # labels

   if (ytext != "None"):
      if ((scale<0.1) or (scale>1000.0)): ytext = ytext + " 1e" + expn 
      ixtext = ixtext - (3*nfont/4) * (len(mant) + 2)       # skip labels + 2
      iytext = (iymin+iymax)/2                              # vertical middle
      w.create_text(ixtext,iytext,text=ytext,font=font2,anchor="e") # y title
                                                                  # draw axes
   if (xmin*xmax < 0): w.create_line(Nint(bx),iymin,Nint(bx),iymax)  # y-axis
   if (ymin*ymax < 0): w.create_line(ixmin,Nint(by),ixmax,Nint(by))  # x-axis

   tic = 2*tic/3
   if (sty == 4): hx = ax * (x[2] - x[1])
   ix0 = Nint(ax*x[1]+bx); iy0 = Nint(ay*y[1]+by)                 # 1st point
   for i in range(1,n+1):
      ix = Nint(ax*x[i]+bx); iy = Nint(ay*y[i]+by)                # new point
      if (sty == 0):                                           # scatter plot
         w.create_rectangle(ix-tic,iy-tic,ix+tic,iy+tic, \
                            fill="",outline=col)
      if (sty == 1 or sty == 2):                         # line or polar plot
         w.create_line(ix0,iy0,ix,iy,fill=col)
      if (sty == 3):                                             # drop lines
         w.create_line(ix,by,ix,iy,fill=col)
      if (sty == 4 and i < n):
         w.create_rectangle(ix+1,iy,ix+hx-1,by,fill="",outline=col)
      ix0 = ix; iy0 = iy                                         # save point

#============================================================================
def MultiPlot(x, y, sig, n, col, sty, nplot, maxint, \
              xminp, xmaxp, ioptx, yminp, ymaxp, iopty, \
              fxmin, fxmax, fymin, fymax, xtext, ytext, title):
#----------------------------------------------------------------------------
#  Plots nplot real functions of one variable given by sets of (x,y) points.
#  The coordinate sets are stored contiguously in the arrays x[] and y[].
#  The x and y-domains are extended to fit at most maxint intervals
#  expressible as d * 10^p, with d = 1, 2, 5 and p integer.
#
#  x[]    - abscissas of tabulation points for all functions
#  y[]    - ordinates of tabulation points
#  sig[]  - error bars of the tabulation points (useful for sty == 4)
#  n[]    - ending index for the individual plots (nmax = n[nplot])
#  col[]  - plot color ("red", "green", "blue" etc.)
#  sty[]  - plot style 0 - scatter plot with squares
#                      1 - line plot;  -1 - dashed line
#                      2 - polar plot; -2 - dashed line
#                      3 - drop lines
#                      4 - error bars; -4 - including line plot
#  nplot  - number of plots
#  maxint - max. number of labeling intervals
#  ioptx  - 0 - resize x-axis automatically
#           1 - resize x-axis based on user interval [xminp,xmaxp]
#  iopty  - 0 - resize y-axis automatically
#           1 - resize y-axis based on user interval [yminp,ymaxp]
#  fxmin  - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
#  fxmax  - max fractional x-limit of viewport
#  fymin  - min fractional y-limit of viewport (0 < fymin < fymax < 1)
#  fymax  - max fractional y-limit of viewport
#  xtext  - x-axis title; for "None" - axis is not labeled
#  ytext  - y-axis title; for "None" - axis is not labeled
#  title  - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size

   nmax = n[nplot]
   if (ioptx):                                              # x-domain limits
      xmin = xminp; xmax = xmaxp                               # input values
   else:
      xmin = min(x[1:nmax+1]); xmax = max(x[1:nmax+1])      # bounding values
   if (iopty):                                              # y-domain limits
      ymin = yminp; ymax = ymaxp                               # input values
   else:
      ymin = min(y[1:nmax+1]); ymax = max(y[1:nmax+1])      # bounding values

      for iplot in range(1,nplot+1):    # extend y-domain for error bar plots
         if (abs(sty[iplot]) == 4):
            i0 = 1 if iplot == 1 else n[iplot-1] + 1
            for i in range(i0,n[iplot]+1):
               ymin = min(ymin,y[i]-sig[i])
               ymax = max(ymax,y[i]+sig[i])

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)   # viewport limits
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)

   if (2 in sty[1:]):                                            # polar plot
      xmin = min(xmin,ymin); xmax = max(xmax,ymax)       # make domain square
      xmax = max(abs(xmin),abs(xmax)); xmin = -xmax
      ymin = xmin; ymax = xmax
      if (ixmax-ixmin > iymin-iymax):                # adjust viewport limits
         c = 0.5*(ixmin+ixmax)
         d = 0.5*(iymin-iymax)
         ixmin = c - d; ixmax = c + d
      if (ixmax-ixmin < iymin-iymax):
         c = 0.5*(iymin+iymax)
         d = 0.5*(ixmax-ixmin)
         iymin = c + d; iymax = c - d

   w.create_rectangle(ixmin,iymax,ixmax,iymin)              # draw plot frame

   nfont = min(int((ixmax-ixmin)/60.) + 3,12)                     # font size
   font1 = ("Helvetica",nfont)                              # axis label font
   font2 = ("Helvetica",Nint(1.2*nfont))                    # axis title font
   font3 = ("Helvetica",Nint(1.4*nfont))                    # plot title font
   if ((ixmax-ixmin) < 3 * (iymin-iymax)):
      w.create_text((ixmin+ixmax)/2,iymax-3*nfont,text=title,font=font3)
   else:
      w.create_text(ixmax,iymax,text=title,font=font2,anchor="ne")
                                                                     # X-AXIS
   (xmin,xmax,scale,nsigd,nintv) = Limits(xmin,xmax,maxint)   # extend limits
   ax = (ixmax-ixmin)/(xmax-xmin)                    # x scaling coefficients
   bx = ixmin - ax*xmin 

   tic =(ixmax-ixmin)/100.                                       # tic length
   h = (xmax-xmin)/nintv; htic = ax * h                       # labeling step
   iytext = iymin + 1.5*nfont 
   for i in range(0,nintv+1):                                    # label axis
      ix = Nint(ixmin + i*htic)
      w.create_line(ix,iymin,ix,iymin-tic)                             # tics
      w.create_line(ix,iymax,ix,iymax+tic)
      if (xtext != "None"):
         (mant,expn) = FormStr(xmin+i*h,scale,nsigd)
         w.create_text(ix,iytext,text=mant,font=font1)               # labels

   if (xtext != "None"):
      if ((scale<0.1) or (scale>1000.0)): xtext = xtext + " 1e" + expn 
      ixtext = (ixmin+ixmax)/2
      iytext = iytext + 2*nfont
      w.create_text(ixtext,iytext,text=xtext,font=font2)            # x title
                                                                     # Y-AXIS
   if (ymin == 0.0 and ymax == 0.0): ymin = -1e0; ymax = 1e0     # horizontal
   if (abs(ymax-ymin) < 1e-5*abs(ymax)): ymin *= 0.9; ymax *= 1.1
   (ymin,ymax,scale,nsigd,nintv) = Limits(ymin,ymax,maxint)   # extend limits
   ay = (iymax-iymin)/(ymax-ymin)                    # y scaling coefficients
   by = iymin - ay*ymin 

   h = (ymax-ymin)/nintv; htic = ay * h                  # labeling step size
   ixtext = ixmin - nfont 
   for i in range(0,nintv+1):                                    # label axis
      iy = Nint(iymin + i*htic)
      w.create_line(ixmin,iy,ixmin+tic,iy)                             # tics
      w.create_line(ixmax,iy,ixmax-tic,iy)
      if (ytext != "None"):
         (mant,expn) = FormStr(ymin+i*h,scale,nsigd)
         w.create_text(ixtext,iy,text=mant,font=font1,anchor="e")    # labels

   if (ytext != "None"):
      if ((scale<0.1) or (scale>1000.0)): ytext = ytext + " 1e" + expn 
      ixtext = ixtext - (3*nfont/4) * (len(mant) + 2)       # skip labels + 2
      iytext = (iymin+iymax)/2                              # vertical middle
      w.create_text(ixtext,iytext,text=ytext,font=font2,anchor="e") # y title
                                                                  # draw axes
   if (xmin*xmax < 0): w.create_line(Nint(bx),iymin,Nint(bx),iymax)  # y-axis
   if (ymin*ymax < 0): w.create_line(ixmin,Nint(by),ixmax,Nint(by))  # x-axis

   tic = 2*tic/3
   for iplot in range(1,nplot+1):
      icol = col[iplot]
      isty = sty[iplot]
      i0 = 1 if iplot == 1 else n[iplot-1] + 1
      ix0 = Nint(ax*x[i0]+bx); iy0 = Nint(ay*y[i0]+by)            # 1st point
      if (isty == 0):
         w.create_rectangle(ix0-tic,iy0-tic,ix0+tic,iy0+tic, \
                            fill="",outline=icol)
      for i in range(i0,n[iplot]+1):
         ix = Nint(ax*x[i]+bx); iy = Nint(ay*y[i]+by)             # new point
         if (isty == 0):                                       # scatter plot
            w.create_rectangle(ix-tic,iy-tic,ix+tic,iy+tic, \
                               fill="",outline=icol)
         if (abs(isty) == 1 or abs(isty) == 2):          # line or polar plot
            if (isty > 0): w.create_line(ix0,iy0,ix,iy,fill=icol)
            else:          w.create_line(ix0,iy0,ix,iy,fill=icol,dash=(4,4))
         if (isty == 3):                                         # drop lines
            w.create_line(ix,by,ix,iy,fill=icol)
         if (abs(isty) == 4):                                    # error bars
            isig = Nint(ay*sig[i])
            w.create_line(ix,iy-isig,ix,iy+isig,fill=icol)
            w.create_line(ix-tic,iy-isig,ix+tic,iy-isig,fill=icol)
            w.create_line(ix-tic,iy+isig,ix+tic,iy+isig,fill=icol)
            if (isty > 0):
               w.create_oval(ix-tic,iy-tic,ix+tic,iy+tic, \
                             fill="white",outline=icol)
            else:
               w.create_line(ix0,iy0,ix,iy,fill=icol)
         ix0 = ix; iy0 = iy                                      # save point

#============================================================================
def RGBcolors(ncolstep):
#----------------------------------------------------------------------------
#  Generates ncol = 1280/ncolsep RGB colors in icol[1] through icol[ncol]
#
#  Returns: icol, ncol
#----------------------------------------------------------------------------
   ncol = int(1280/ncolstep)
   icol = [0]*(ncol+1)

   i = 0
   bc = 255; rc = 0
   for gc in range(0,256,ncolstep):
      i += 1; icol[i] = "#%02x%02x%02x" % (rc,gc,bc)

   gc = 255; rc = 0
   for bc in range(255,-1,-ncolstep):
      i += 1; icol[i] = "#%02x%02x%02x" % (rc,gc,bc)

   bc = 0; gc = 255
   for rc in range(0,256,ncolstep):
      i += 1; icol[i] = "#%02x%02x%02x" % (rc,gc,bc)

   bc = 0; rc = 255
   for gc in range(255,-1,-ncolstep):
      i += 1; icol[i] = "#%02x%02x%02x" % (rc,gc,bc)

   gc = 0; rc = 255
   for bc in range(0,256,ncolstep):
      i += 1; icol[i] = "#%02x%02x%02x" % (rc,gc,bc)

   return (icol, ncol)

#============================================================================
def ColorLegend(fmin, fmax, ixmin, ixmax, iymin, iymax):
#----------------------------------------------------------------------------
#  Draws and labels the color legend for the interval [fmin,fmax] in the
#  rectangle [ixmin,ixmax] x [iymin,iymax]. [fmin,fmax] is extended to fit at
#  most 10 intervals expressible as d * 10^p, with d = 1, 2, 5 and p integer.
#
#  Returns: fmin, fmax, icol, ncol, nintv
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size

   nfont = min(int((iymin-iymax)/20.),12)
   font = ("Helvetica",nfont)                                    # label font

   ncolstep = 2
   (icol,ncol) = RGBcolors(ncolstep)

   hcol = float(ncol) / (iymax - iymin)
   for iy in range(iymax,iymin+1):
      ic = min(max(1,int((iy-iymin)*hcol)),ncol)
      w.create_line(ixmin,iy,ixmax,iy,fill=icol[ic])
   w.create_rectangle(ixmin,iymin,ixmax,iymax)
   
   (fmin,fmax,scale,nsigd,nintv) = Limits(fmin,fmax,10)       # extend limits
   ay = (iymax-iymin)/(fmax-fmin)                      # scaling coefficients
   by = iymin - ay*fmin
   h = (fmax-fmin)/nintv; htic = ay * h                  # labeling step size
   ixtext = ixmax + (nsigd+1)*nfont 
   for i in range(0,nintv+1):                                  # label legend
      iy = Nint(iymin + i*htic)
      (mant,expn) = FormStr(fmin+i*h,scale,nsigd)
      w.create_text(ixtext,iy,text=mant,font=font,anchor="e")        # labels

   if ((scale<0.1) or (scale>1000.0)):
      ytext = " x 1e" + expn 
      w.create_text(ixtext,iymin+2*nfont,text=ytext,font=font,anchor="e")

   return (fmin, fmax, icol, ncol, nintv)

#============================================================================
def Contour(z, nx, ny, xmin, xmax, ymin, ymax, zmin, zmax, \
            fxmin, fxmax, fymin, fymax, xtext, ytext, title):
#----------------------------------------------------------------------------
#  Plots a function z(x,y) defined in [xmin,xmax] x [ymin,ymax] and tabulated
#  on a regular Cartesian grid with (nx-1)x(ny-1) mesh cells as contour plot.
#  The level curves result by inverse linear interpolation inside the cells.
#
#  z     - tabulated function values
#  nx    - number of x-mesh points
#  ny    - number of y-mesh points
#  zmin  - minimum level considered
#  zmin  - maximum level considered
#  fxmin - minimum relative viewport abscissa (0 < fxmin < fxmax < 1)
#  fxmax - maximum relative viewport abscissa
#  fymin - minimum relative viewport ordinate (0 < fymin < fymax < 1)
#  fymax - maximum relative viewport ordinate
#  xtext - x-axis title; xtext = "" - axis is not labeled
#  ytext - y-axis title; ytext = "" - axis is not labeled
#  title - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size
   xg = [0]*8; yg = [0]*8

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)   # viewport coords
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)

   f = (xmax-xmin)/(ymax-ymin)                # scale viewport proportionally
   if (f < float(ixmax-ixmin)/(iymin-iymax)):                # shorter x-axis
      ixmax = ixmin + ixmax                         # correct ixmin and ixmax
      ixmin = int(0.5e0*(ixmax + (iymax-iymin)*f))
      ixmax = ixmax - ixmin
   else:                                                     # shorter y-axis
      iymax = iymin + iymax                         # correct iymin and iymax
      iymin = int(0.5e0*(iymax + (ixmax-ixmin)/f))
      iymax = iymax - iymin

   nfont = min(int((ixmax-ixmin)/60.) + 3,12)                     # font size
   font1 = ("Helvetica",nfont)                              # axis label font
   font2 = ("Helvetica",Nint(1.2*nfont))                    # axis title font
   font3 = ("Helvetica",Nint(1.4*nfont))                    # plot title font
   w.create_text((ixmin+ixmax)/2,iymax-3*nfont,text=title,font=font3)

   iyc = (iymax + iymin)/2.                                    # color legend
   iyd = min((ixmax - ixmin + iymin - iymax)/8.,(iymin - iymax)/3.)
   ixd = iyd/5.
   ix1 = Nint(ixmax +   ixd); iy1 = Nint(iyc + iyd)
   ix2 = Nint(ixmax + 2*ixd); iy2 = Nint(iyc - iyd)
   (zmin,zmax,icol,ncol,nintv) = ColorLegend(zmin,zmax,ix1,ix2,iy1,iy2)
   if (nintv <= 6): nintv *= 2

   hx = (ixmax-ixmin)/(nx-1e0)                                    # draw grid
   hy = (iymax-iymin)/(ny-1e0)
   hz = (zmax-zmin)/(ncol-1e0)
   hv = (zmax-zmin)/nintv
   for iz in range(1,ncol+nintv+1):                        # loop over levels
      if (iz <= ncol):                                         # color shades
         z0 = zmin + (iz-1)*hz
         col = icol[iz]
      else:                                                           # lines
         z0 = zmin + (iz-ncol)*hv
      for i in range(1,nx):                          # loop over cells [i][j]
         for j in range(1,ny):                           # corner coordinates
            ix = Nint(ixmin + (i-1)*hx); ixh = Nint(ixmin + i*hx)
            iy = Nint(iymin + (j-1)*hy); iyh = Nint(iymin + j*hy)
            z1 = z[i][j]   - z0; z2 = z[i+1][j]   - z0   # rel. corner values
            z3 = z[i][j+1] - z0; z4 = z[i+1][j+1] - z0

            if (z1 < 0e0 or z2 < 0e0 or z3 < 0e0 or z4 < 0e0):
               ng = 0                    # level line passes through the cell
               if (z1*z2 <= 0):                # intersection with lower edge
                  if (z1 != z2):
                     ng += 1; xg[ng] = Nint(ix + z1/(z1-z2)*hx); yg[ng] = iy
                  else:                            # line coincides with edge
                     ng += 1; xg[ng] = ix ; yg[ng] = iy
                     ng += 1; xg[ng] = ixh; yg[ng] = iy

               if (z1*z3 <= 0):                 # intersection with left edge
                  if (z1 != z3):
                     ng += 1; xg[ng] = ix; yg[ng] = Nint(iy + z1/(z1-z3)*hy)
                  else:                            # line coincides with edge
                     ng += 1; xg[ng] = ix; yg[ng] = iy
                     ng += 1; xg[ng] = ix; yg[ng] = iyh

               if (z2*z4 <= 0):                # intersection with right edge
                  if (z2 != z4):
                     ng += 1; xg[ng] = ixh; yg[ng] = Nint(iy + z2/(z2-z4)*hy)
                  else:                            # line coincides with edge
                     ng += 1; xg[ng] = ixh; yg[ng] = iy
                     ng += 1; xg[ng] = ixh; yg[ng] = iyh

               if (z3*z4 <= 0):                # intersection with upper edge
                  if (z3 != z4):
                     ng += 1; xg[ng] = Nint(ix + z3/(z3-z4)*hx); yg[ng] = iyh

               if (iz <= ncol):
                  polygon0 = ()
                  if (z1 >= 0e0): polygon0 += ix , iy
                  if (z2 >= 0e0): polygon0 += ixh, iy
                  if (z3 >= 0e0): polygon0 += ix , iyh
                  if (z4 >= 0e0): polygon0 += ixh, iyh
                  for ig in range(1,ng+1):     # fill the polygon above level
                     for jg in range(1,ng):
                        polygon = polygon0 + (xg[ig],yg[ig],xg[jg],yg[jg])
                        w.create_polygon(polygon,fill=col,outline=col)
               else:
                  for ig in range(1,ng+1):                 # draw levele line
                     for jg in range(1,ng):
                        w.create_line(xg[ig],yg[ig],xg[jg],yg[jg])
            else:         # all corner values exceed level - fill entire cell
               if (iz <= ncol):
                  w.create_rectangle(ix,iy,ixh,iyh,fill=col,outline=col)

   w.create_rectangle(ixmin,iymax,ixmax,iymin)                  # draw border
                                                                     # X-AXIS
   maxint = 10
   (xmin1,xmax1,scale,nsigd,nintv) = Limits(xmin,xmax,maxint) # extend limits
   h = (xmax1-xmin1)/nintv
   if (xmin1 < xmin): xmin1 += h; nintv -= 1
   if (xmax1 > xmax): xmax1 -= h; nintv -= 1
   ax = (ixmax-ixmin)/(xmax-xmin)                      # scaling coefficients
   bx = ixmin - ax*xmin 

   tic =(ixmax-ixmin)/100.                                       # tic length
   h = (xmax1-xmin1)/nintv                                    # labeling step
   iytext = iymin + 1.5*nfont 
   for i in range(0,nintv+1):                                    # label axis
      xi = xmin1 + i*h
      ix = Nint(ax*xi+bx)
      w.create_line(ix,iymin,ix,iymin-tic)                             # tics
      w.create_line(ix,iymax,ix,iymax+tic)
      if (xtext):
         (mant,expn) = FormStr(xi,scale,nsigd)
         w.create_text(ix,iytext,text=mant,font=font1)               # labels

   if ((scale<0.1) or (scale>1000.0)): xtext = xtext + " x 1e" + expn 
   ixtext = (ixmin+ixmax)/2
   iytext = iytext + 2*nfont
   w.create_text(ixtext,iytext,text=xtext,font=font2)            # axis title
                                                                     # Y-AXIS
   (ymin1,ymax1,scale,nsigd,nintv) = Limits(ymin,ymax,maxint) # extend limits
   h = (ymax1-ymin1)/nintv
   if (ymin1 < ymin): ymin1 += h; nintv -= 1
   if (ymax1 > ymax): ymax1 -= h; nintv -= 1
   ay = (iymax-iymin)/(ymax-ymin)                      # scaling coefficients
   by = iymin - ay*ymin 

   h = (ymax1-ymin1)/nintv                               # labeling step size
   ixtext = ixmin - nfont 
   for i in range(0,nintv+1):                                    # label axis
      yi = ymin1 + i*h
      iy = Nint(ay*yi+by)
      w.create_line(ixmin,iy,ixmin+tic,iy)                             # tics
      w.create_line(ixmax,iy,ixmax-tic,iy)
      if (ytext):
         (mant,expn) = FormStr(yi,scale,nsigd)
         w.create_text(ixtext,iy,text=mant,font=font1,anchor="e")    # labels

   if ((scale<0.1) or (scale>1000.0)): ytext = ytext + " x 1e" + expn 
   ixtext = ixtext - (3*nfont/4) * (len(mant) + 2)          # skip labels + 2
   iytext = (iymin+iymax)/2                                 # vertical middle
   w.create_text(ixtext,iytext,text=ytext,font=font2,anchor="e") # axis title

#============================================================================
def PlotParticles(x, y, z, r, col, n, dmax, \
                  xminp, xmaxp, ioptx, yminp, ymaxp, iopty, \
                  fxmin, fxmax, fymin, fymax, title):
#----------------------------------------------------------------------------
#  Plots a system of particles as connected colored spheres
#
#  x,y,z[] - coordinates of particles
#  r[]     - radii of particles
#  col[]   - colors of particles ("red", "green", "blue" etc.)
#  n       - number of particles
#  dmax    - max inter-distance for which particles are connected
#  ioptx   - 0 - resize x-axis automatically
#            1 - resize x-axis to provided user interval (xminp,xmaxp)
#  iopty   - 0 - resize y-axis automatically
#            1 - resize y-axis to provided user interval (yminp,ymaxp)
#  fxmin   - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
#  fxmax   - max fractional x-limit of viewport
#  fymin   - min fractional y-limit of viewport (0 < fymin < fymax < 1)
#  fymax   - max fractional y-limit of viewport
#  title   - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size
   ind = [0]*(n+1)

   if (ioptx):                                              # x-domain limits
      xmin = xminp; xmax = xmaxp                               # input values
   else:
      xmin = min(x[1:]); xmax = max(x[1:])                  # bounding values
   if (iopty):                                              # y-domain limits
      ymin = yminp; ymax = ymaxp                               # input values
   else:
      ymin = min(y[1:]); ymax = max(y[1:])                  # bounding values

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)   # viewport limits
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)

   nfont = int((ixmax-ixmin)/20.)                                 # font size
   font = ("Helvetica",nfont)                                    # title font
   w.create_text((ixmin+ixmax)/2,iymax-3*nfont,text=title,font=font)  # title

   f = (xmax-xmin)/(ymax-ymin)                # scale viewport proportionally
   if (f < float(ixmax-ixmin)/(iymin-iymax)):                # shorter x-axis
      ixmax = ixmin + ixmax                          # adjust ixmin and ixmax
      ixmin = int(0.5*(ixmax + (iymax-iymin)*f))
      ixmax = ixmax - ixmin
   else:                                                     # shorter y-axis
      iymax = iymin + iymax                          # adjust iymin and iymax
      iymin = int(0.5*(iymax + (ixmax-ixmin)/f))
      iymax = iymax - iymin

   ax = (ixmax-ixmin)/(xmax-xmin); bx = ixmin - ax*xmin      # scaling coeffs
   ay = (iymax-iymin)/(ymax-ymin); by = iymin - ay*ymin
                                                                 
   for i1 in range(1,n+1):                                       # draw bonds
      for i2 in range(1,i1):
         dx = x[i1] - x[i2]; dy = y[i1] - y[i2]; dz = z[i1] - z[i2]
         d = sqrt(dx*dx + dy*dy + dz*dz)
         if ( d <= dmax):
            ix1 = Nint(ax*x[i1]+bx); iy1 = Nint(ay*y[i1]+by)
            ix2 = Nint(ax*x[i2]+bx); iy2 = Nint(ay*y[i2]+by)
            w.create_line(ix1,iy1,ix2,iy2,fill="slate gray",width=10)
            w.create_line(ix1,iy1,ix2,iy2,fill="gray",width=6)
            w.create_line(ix1,iy1,ix2,iy2,fill="white",width=2)

   Index(z,ind,n)                  # index particles in increasing order of z

   for i in range(1,n+1):           # draw particles in increasing order of z
      i1 = ind[i]
      ix = Nint(ax*x[i1]+bx); iy = Nint(ay*y[i1]+by)
      rgb = w.winfo_rgb(col[i1])         # rgb representation of dimmed color
      (rc, gc, bc) = (int(rgb[0]/512), int(rgb[1]/512), int(rgb[2]/512))
      r0 = int(ax * r[i1])                                  # starting radius
      for j in range(r0,0,-1):                              # decrease radius
          rgb = '#%02x%02x%02x' % (rc, gc, bc)          # increase luminosity
          w.create_oval(ix-j,iy-j,ix+j,iy+j,fill=str(rgb),outline=str(rgb))
          (rc, gc, bc) = (min(255,rc+6), min(255,gc+6), min(255,bc+6))

#============================================================================
def PlotStruct(x, y, z, n, ind1, ind2, ind3, n3, \
               xminp, xmaxp, ioptx, yminp, ymaxp, iopty, \
               fxmin, fxmax, fymin, fymax, title):
#----------------------------------------------------------------------------
#  Renders a 3D structure defined by nodes and triangular surfaces
#
#  x,y,z[] - coordinates of nodes
#  n       - number of nodes
#  ind1[]  - index of 1st node of each triangle
#  ind2[]  - index of 2nd node of each triangle
#  ind3[]  - index of 3rd node of each triangle
#  n3      - number of triangles
#  ioptx   - 0 - resize x-axis automatically
#            1 - resize x-axis to provided user interval (xminp,xmaxp)
#  iopty   - 0 - resize y-axis automatically
#            1 - resize y-axis to provided user interval (yminp,ymaxp)
#  fxmin   - min fractional x-limit of viewport (0 < fxmin < fxmax < 1)
#  fxmax   - max fractional x-limit of viewport
#  fymin   - min fractional y-limit of viewport (0 < fymin < fymax < 1)
#  fymax   - max fractional y-limit of viewport
#  title   - plot title
#----------------------------------------------------------------------------
   global w, nxw, nyw                                # canvas object and size
   zmax = [0]*(n3+1)
   cosn = [0]*(n3+1)
   ind  = [0]*(n3+1)
 
   if (ioptx):                                              # x-domain limits
      xmin = xminp; xmax = xmaxp                               # input values
   else:
      xmin = min(x[1:]); xmax = max(x[1:])                  # bounding values
   if (iopty):                                              # y-domain limits
      ymin = yminp; ymax = ymaxp                               # input values
   else:
      ymin = min(y[1:]); ymax = max(y[1:])                  # bounding values

   ixmin = Nint(fxmin*nxw); iymin = Nint((1e0-fymin)*nyw)   # viewport coords
   ixmax = Nint(fxmax*nxw); iymax = Nint((1e0-fymax)*nyw)

   nfont = int((ixmax-ixmin)/20.)                                 # font size
   font = ("Helvetica",nfont)                                    # title font
   w.create_text((ixmin+ixmax)/2,iymax-3*nfont,text=title,font=font)  # title

   f = (xmax-xmin)/(ymax-ymin)                # scale viewport proportionally
   if (f < float(ixmax-ixmin)/(iymin-iymax)):                # shorter x-axis
      ixmax = ixmin + ixmax                         # correct ixmin and ixmax
      ixmin = int(0.5*(ixmax + (iymax-iymin)*f))
      ixmax = ixmax - ixmin
   else:                                                     # shorter y-axis
      iymax = iymin + iymax                         # correct iymin and iymax
      iymin = int(0.5*(iymax + (ixmax-ixmin)/f))
      iymax = iymax - iymin

   ax = (ixmax-ixmin)/(xmax-xmin); bx = ixmin - ax*xmin      # scaling coeffs
   ay = (iymax-iymin)/(ymax-ymin); by = iymin - ay*ymin
                                                                 
   for i in range(1,n3+1):                    # max z and cos(n) of triangles
      i1 = ind1[i]; i2 = ind2[i]; i3 = ind3[i]

      zmax[i] = max(z[i1],z[i2],z[i3])                    # max z of triangle

      ux = x[i2] - x[i1]; vx = x[i3] - x[i1]               # defining vectors
      uy = y[i2] - y[i1]; vy = y[i3] - y[i1]
      uz = z[i2] - z[i1]; vz = z[i3] - z[i1]

      nx = uy * vz - uz * vy                             # normal to triangle
      ny = uz * vx - ux * vz
      nz = ux * vy - uy * vx
      if (nz < 0e0): nx = -nx; ny = -ny; nz= - nz # choose face toward viewer
      cosn[i] = (nx + ny + nz) / sqrt(3e0*(nx*nx + ny*ny + nz*nz))

   Index(zmax,ind,n3)              # index triangles in increasing order of z

   for i in range(1,n3+1):              # draw triangles - the remotest first
      indi = ind[i]
      i1 = ind1[indi]; i2 = ind2[indi]; i3 = ind3[indi]
      ix1 = Nint(ax*x[i1]+bx); iy1 = Nint(ay*y[i1]+by)
      ix2 = Nint(ax*x[i2]+bx); iy2 = Nint(ay*y[i2]+by)
      ix3 = Nint(ax*x[i3]+bx); iy3 = Nint(ay*y[i3]+by)
      polygon = ix1, iy1, ix2, iy2, ix3, iy3, ix1, iy1
      d = int(128 * cosn[indi])
      rc = 64 + d; gc = 64 + d; bc = min(128 + d,255)
      col = str('#%02x%02x%02x' % (rc, gc, bc))
      w.create_polygon(polygon,fill=col,outline="slate gray",width=1)

#============================================================================
def HistoBin(xnew, a, b, x, y, n, iopt):
#----------------------------------------------------------------------------
#  Bins data for a histogram to be plotted by function Plot (with sty = 4)
#
#  xnew - new value to be binned
#  a, b - limits of total binning interval
#  x[]  - bin boundaries (x[1] = a, x[n] = b)
#  y[]  - frequency of values in the bins: y[i] in [x[i],x[i+1]), i = 1,n-1
#  n    - number of bin boundaries
#  iopt - option: 0 - zeros bins
#                 1 - bins new value xnew
#                 2 - normalizes histogram
#----------------------------------------------------------------------------
   h = (b-a)/(n-1)                                                 # bin size

   if (iopt == 0):                                          # initialize bins
      for i in range(1,n+1):
         x[i] = a + (i-1)*h                                  # bin boundaries
         y[i] = 0e0

   elif (iopt == 1):                                         # bin new values
      i = (int) ((xnew-a)/h) + 1                                  # bin index
      if ((i >= 1) and (i < n)): y[i] += 1              # increment bin value

   elif (iopt == 2):                                    # normalize histogram
      s = 0e0
      for i in range(1,n): s += y[i]         # sum of unnormalized bin values
      for i in range(1,n): y[i] /= s                   # normalize bin values
