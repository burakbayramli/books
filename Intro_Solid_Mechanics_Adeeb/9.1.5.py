import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider
from mpl_toolkits.mplot3d import Axes3D
%matplotlib notebook
class plot3dCube(object):
    def __init__(self, origin, length, color):
        # origin[x,y,z], length[Lx,Ly,Lz], 
        self.g = 1
        self.origin = origin
        self.length = length
        #initial plot
        self.updateCube(origin,length)
        self.height = length[2]
        # create plot figure
        self.fig = plt.figure(figsize=(6,7))
        self.ax = self.fig.add_subplot(111, projection='3d')
        self.ax.set_xlim3d(-1, 0)
        self.ax.set_ylim3d(-1, 0)
        self.ax.set_zlim3d(-5, 10)
        self.ax.set_xlabel('X')
        self.ax.set_ylabel('Y')
        self.ax.set_zlabel('Z')
        # set color 
        self.color = color
        # dictionary of surfaces and edges
        self.dFrm = {}
        # surface variable name
        surface = "self.surface"
        # edge variable name
        edge = "self.edge"
        # generate frame and edge plots
        for i in range(len(self.sqrP)):
            # number
            n = str(i)
            self.dFrm[surface+n], self.dFrm[edge+n] = self.initFrame(self.sqrP[i][:])
        # slider plot
        # adjust plot
        plt.subplots_adjust(left = 0.1, bottom = 0.45)
        # input the position of sliders
        xSlider = plt.axes([0.2,0.2,0.5,0.03])
        ySlider = plt.axes([0.2,0.15,0.5,0.03])
        zSlider = plt.axes([0.2,0.1,0.5,0.03])
        xSlider.set_xticks(np.array([90,180,270]), minor = True)
        ySlider.set_xticks(np.array([1,2,3,4]), minor = True)
        zSlider.set_xticks(np.array([1,2,3,4]), minor = True)
        # input values and update graph
        self.slider1 = Slider(xSlider, 't', 0, 4*np.pi*np.sqrt(5), valinit = 0, valstep = 0.1)
        self.slider2 = Slider(ySlider, 'm', 1, 5, valinit = 1, valstep = 0.1)
        self.slider3 = Slider(zSlider, 'k', 1, 5, valinit = 1, valstep = 0.1)
        # updates the graph when input is changes
        self.slider1.on_changed(self.updateGraph)
        self.slider2.on_changed(self.updateGraph)
        self.slider3.on_changed(self.updateGraph)
    def updateCube(self,origin, length):
        Lx,Ly,Lz = length
        x1,y1,z1 = origin
        x2,y2,z2 = [length[i]+origin[i] for i in range(3)]
        # for A as X,Y,Z
        # A_1212 uses [[x1, x2][x1,x2]]
        # A_1122 is just X_1.T
        # A_1111 all x1
        # A_2222 all x2
        r = [x1, x2]
        X_1212 = np.array([r,r])
        X_1111 = np.ones(4).reshape(2, 2)*x1
        X_2222 = np.ones(4).reshape(2, 2)*x2
        r = [y1, y2]
        Y_1212, Y_1122 = np.meshgrid(r, r)
        Y_1111 = np.ones(4).reshape(2, 2)*y1
        Y_2222 = np.ones(4).reshape(2, 2)*y2
        r = [z1, z2]
        Z_1122 = np.array([r,r]).transpose()
        Z_1111 = np.ones(4).reshape(2, 2)*z1
        Z_2222 = np.ones(4).reshape(2, 2)*z2
        # points of the cube
        self.points = np.array([[x1, y1, z1],
                               [x2, y1, z1],
                               [x2, y2, z1],
                               [x1, y2, z1],
                               [x1, y1, z2],
                               [x2, y1, z2],
                               [x2, y2, z2],
                               [x1, y2, z2]])
        # square points for surface, edges
        self.sqrP = [[X_1212,Y_1122,Z_1111],[X_1212,Y_1122,Z_2222],
                     [X_1212,Y_1111,Z_1122],[X_1212,Y_2222,Z_1122],
                     [X_1111,Y_1212,Z_1122],[X_2222,Y_1212,Z_1122]]
    def initFrame(self, sqrP):
        surface = self.ax.plot_surface(sqrP[0],sqrP[1],sqrP[2],alpha=0.5,color=self.color[0])
        edge = self.ax.plot_wireframe(sqrP[0],sqrP[1],sqrP[2],alpha=0.5,color=self.color[1])
        return surface, edge
    def updateGraph(self, event):
        t, m, k = self.slider1.val, self.slider2.val, self.slider3.val
        g = self.g
        z = (g*m-g*m*np.cos(np.sqrt(k/m)*t))/k
        self.origin[2] = z
        self.length[2] = self.height - z
        self.updateCube(self.origin, self.length)
        self.updateFrames()
    def updateFrame(self, surface, edge, X, Y, Z):
        #destory previous surfaces and edges
        surface.remove()
        edge.remove()
        # plot surfaces and edges
        surface = self.ax.plot_surface(X,Y,Z, alpha = 0.5, color = self.color[0])
        edge = self.ax.plot_wireframe(X,Y,Z, alpha = 0.5, color = self.color[1])
        return surface, edge
    def updateFrames(self):        
        surface = "self.surface"
        edge = "self.edge"
        # destroy current surface and edge, new surface and edge
        for j in range(6):
            n = str(j)
            self.dFrm[surface+n], self.dFrm[edge+n]= self.updateFrame(self.dFrm[surface+n],self.dFrm[edge+n], 
                                                             self.sqrP[j][0],self.sqrP[j][1],self.sqrP[j][2])
        plt.draw()
        plt.pause(.01)
# main function
p = plot3dCube([0,0,0], [-1,-1,-5],["grey","black"])
