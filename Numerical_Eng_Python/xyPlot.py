# module xyPlot
''' xyPlot(x1,y1,type1,x2,y2,type2,...)
    Plots y1 vs. x1, y2 vs. x2,... where x1,y1,
    etc. are arrays.
    type1, type2,... are two-character strings that specify
    the appearance of each plot as follows:
    If first character = 'l' points are connected by lines
                       = 'n' points are not connected
    If second character = 'o' points are displayed as circles
                        = 'b' points are displayed as boxes
                        = 'n' points are not displayed
   '''
from Tkinter import *
from Canvas import Rectangle,Oval,Line,CanvasText
from math import log10

def xyPlot(*data):
     
    def dataRange(data):
        # Finds minimum and maximum values of data
        xMin = 1.0e12
        xMax = -1.0e12
        yMin = 1.0e12
        yMax = -1.0e12
        n = len(data)/3
        for i in range(n):
            xData = data[3*i-3]
            yData = data[3*i-2]
            x1 = min(xData)
            if x1 < xMin: xMin = x1
            x2 = max(xData)
            if x2 > xMax: xMax = x2
            y1 = min(yData)
            if y1 < yMin: yMin = y1
            y2 = max(yData)
            if y2 > yMax: yMax = y2
        if yMin == yMax:
            if   yMin > 0.0: yMin = 0.0
            elif yMin < 0.0: yMax = 0.0
            else: yMax = 1.0
        return xMin,xMax,yMin,yMax
    
    def viewport(aMin,aMax):
        # Computes the size of the viewport
        logE = 0.4342945
        np = int(log10(abs(aMax - aMin))) - 1
        p = 10**np
        aMin = round(aMin/p - 0.49)*p
        aMax = round(aMax/p + 0.49)*p
        return aMin,aMax,p

    def xMap(xMin,xMax):
        # Computes parameters used in mapping x to
        # screen coordinates: xPixel = A*x + B*x*x
        Ax = leftMargin - width*xMin/(xMax - xMin)
        Bx = width/(xMax - xMin)
        return Ax,Bx
    
    def yMap(yMin,yMax):
        # Computes parameters used in mapping y to
        # screen coordinates: yPixel = Ay + By*y
        Ay = topMargin + height*yMax/(yMax - yMin)
        By = -height/(yMax - yMin)
        return Ay,By

    def tickSpace(aMin,aMax,p):
        # Computes tick spacing for the axes
        n = (aMax - aMin)/p
        if n <= 10: da = 1.0
        elif (n > 11) and (n <= 20): da = 2.0
        elif (n >= 21) and (n <= 40): da = 5.0
        elif (n >= 41) and (n <= 70): da = 10.0
        else: da = 20.0
        return da*p

    def xTicks(xMin,xMax,Ax,Bx):
        # Plot ticks and labels for x-axis
        dx = tickSpace(xMin,xMax,px)
        yp = topMargin + height
        if xMin < 0.0: 
            x = 0.0
            while x >= xMin:
                if x <= xMax:
                    xp = Ax + Bx*x    
                    Line(cv,xp,yp,xp,yp - 8)
                    label = str(round(x,4))
                    CanvasText(cv,xp,yp+20,text=label)
                x = x - dx    
        if xMax > 0.0:
            x = 0.0
            while x <= xMax:
                if x >= xMin:
                    xp = Ax + Bx*x             
                    Line(cv,xp,yp,xp,yp - 8)
                    label = str(round(x,4))
                    CanvasText(cv,xp,yp+20,text=label)
                x = x + dx

    def yTicks(yMin,yMax,Ay,By):
        # Plot ticks and labels for y-axis
        dy = tickSpace(yMin,yMax,py)
        xp = leftMargin
        if yMin < 0.0: 
            y = 0.0
            while y >= yMin:
                if y <= yMax:
                    yp = Ay + By*y
                    Line(cv,xp,yp,xp+8,yp)
                    label = str(round(y,4))
                    xxp = xp-10-len(label)*3
                    CanvasText(cv,xxp,yp,text=label)
                y = y - dy 
        if yMax > 0.0:
            y = 0.0
            while y <= yMax:
                if y >= yMin:
                    yp = Ay + By*y
                    Line(cv,xp,yp,xp+8,yp)
                    label = str(round(y,4))
                    xxp = xp-10-len(label)*3
                    CanvasText(cv,xxp,yp,text=label)
                y = y + dy
        
    def axes(xMin,xMax,yMin,yMax,Ax,Ay):
        # Draws the x = 0 and y = 0 lines
        if xMin*xMax < 0:
            Line(cv,Ax,topMargin,Ax,topMargin+height)
        if yMin*yMax < 0:
            Line(cv,leftMargin,Ay,leftMargin+width,Ay)
            
    def plotting(xData,yData,symb,Ax,Bx,Ay,By):
        # Plots the data
        global cv
        for i in range(len(xData)-1):
            x1 = round(Ax + Bx*xData[i])
            y1 = round(Ay + By*yData[i])
            x2 = round(Ax + Bx*xData[i+1])
            y2 = round(Ay + By*yData[i+1])
            if symb[0] == 'l': Line(cv,x1,y1,x2,y2)
            if symb[1] == 'o':
                Oval(cv,x1-3,y1-3,x1+3,y1+3,fill="white")
            if symb[1] == 'b':
                Rectangle(cv,x1-3,y1-3,x1+3,y1+3,fill="white")
        if symb[1] == 'o':
                Oval(cv,x2-3,y2-3,x2+3,y2+3,fill="white")
        if symb[1] == 'b':
                Rectangle(cv,x2-3,y2-3,x2+3,y2+3,fill="white") 

    W=640   # Width of plot window
    H=480   # Height of plot window
    leftMargin = 100
    topMargin = 25
    width = W - 150
    height = H - 75
    global cv
    root = Tk()
    cv = Canvas(root,width=W,height=H)
    cv.pack()
    Rectangle(cv,leftMargin,topMargin,width+leftMargin,\
              height+topMargin)
    [xMin,xMax,yMin,yMax] = dataRange(data)
    [xMin,xMax,px] = viewport(xMin,xMax)
    [yMin,yMax,py] = viewport(yMin,yMax)
    [Ax,Bx] = xMap(xMin,xMax)
    [Ay,By] = yMap(yMin,yMax)
    axes(xMin,xMax,yMin,yMax,Ax,Ay)
    xTicks(xMin,xMax,Ax,Bx)
    yTicks(yMin,yMax,Ay,By)
    n = len(data)/3
    for i in range(n):
        xData = data[3*i-3]
        yData = data[3*i-2]
        symb = data[3*i-1]
        plotting(xData,yData,symb,Ax,Bx,Ay,By)
    
