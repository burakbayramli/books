# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
"""
Frozen development
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines as lines
import matplotlib.patches as patches
import matplotlib.collections as collections

class GSpring(lines.Line2D):
    def __init__(self,init,**kwargs):
        lines.Line2D.__init__(self,[],[],**kwargs)
        self.xdata = np.array([0,1/5,2/5,3/5,4/5,1])+init
        self.ydata = np.array([0,0,0.05,-0.05,0,0])
        # Set data
        self.set_xdata(self.xdata)
        self.set_ydata(self.ydata)
        #self.set_linestyle("-") #Solid line
        
class GFixedNode(collections.PatchCollection):
    def __init__(self,xcoord,ycoord,**kwargs):
        collections.PatchCollection.__init__(self,[],**kwargs)
        tol = 0.02
        _xdata1 = np.array([xcoord-tol,xcoord,xcoord+tol])
        _ydata1 = np.array([ycoord-tol,ycoord,ycoord-tol])
        _xdata2 = np.array([xcoord-tol,xcoord,xcoord-tol])
        _ydata2 = np.array([ycoord-tol,ycoord,ycoord+tol])
        # Polygons
        p1 = patches.Polygon(list(zip(_xdata1,_ydata1)))
        p1.set_color("r")
        p2 = patches.Polygon(list(zip(_xdata2,_ydata2)))
        #print p1,p2
        #p2.set_color("g")
        # Set data
        self.set_paths((p1,p2))
        self.set_color("k")
        #self.set_marker("-")
        #self.set_mfc('r')
        # self.set_ms(10)
        
def plot_spring_model(model):
    GRAPH_TOL = 0.1
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    for k in model.elements.keys():
        ax.add_line(GSpring(k))
    
    #~ # BCs
    for k,v in model.U.items():
        if v["ux"]==0:
            ax.add_collection(GFixedNode(k,0))
    
    max_x = len(model.elements)
    max_y = 0.2
    ax.set_xlim(-GRAPH_TOL, max_x+GRAPH_TOL)
    ax.set_ylim(-max_y,max_y)
    #~ fig.savefig('this.png')
    plt.show()
    

def plot_truss_model(model):
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    for node in model.nodes.values():
        ax.plot(node.x, node.y, "ko")
    
    for element in model.elements.values():
        na, nb = element.getNodes()
        xx = [model.nodes[na.label].x, model.nodes[nb.label].x]
        yy = [model.nodes[na.label].y, model.nodes[nb.label].y]
        ax.plot(xx, yy, color="#5050dd")
    
    minx, maxx = xlim(model)
    miny, maxy = ylim(model)
    tolx = (maxx-minx)/10.0
    toly = (maxy-miny)/10.0
    ax.set_xlim(minx - tolx, maxx + tolx)
    ax.set_ylim(miny - toly, maxy + toly)
    plt.show()

def xlim(model):
    xcoords = [node.x for node in model.nodes.values()]
    return (min(xcoords),max(xcoords))
    
def ylim(model):
    ycoords = [node.y for node in model.nodes.values()]
    return (min(ycoords),max(ycoords))


if __name__ == '__main__':
    plot_spring_model()
