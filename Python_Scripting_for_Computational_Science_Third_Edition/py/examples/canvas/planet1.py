#!/usr/bin/env python
from Tkinter import *
import math, copy, time
from scitools.CanvasCoords import CanvasCoords
C = CanvasCoords()  # transformation object
# default values
C.set_coordinate_system(400,800,200,200)

from model1 import *

class Planet:
    def __init__(self, x, y, radius=10, color='red', canvas=None):
        self.x = x; self.y = y;   # current physical center coordinates
        self.rc = radius;         # radius in canvas coords
        self.r = float(radius/C.xy_scale); # radius in physical coords
        self.color = color;
        self.id = 0;              # ID as returned from create_oval
        if canvas is not None:
            self.draw(canvas)
        
    def draw(self, canvas):
        # create a circle: the canvas oval command specifies an
        # ellipse enclosed in a rectangle with opposite corners
        # upper-left (x1,y1) and lower-right (x2,y2):        
        c = C.physical2canvas4(self.get_corners())
        self.id = canvas.create_oval(c[0],c[1],c[2],c[3],
                                     fill=self.color, outline=self.color)
                                     #tag='moveable')
        # self.id can be used to identify the object later
        self.canvas = canvas

    def get_corners(self): 
        # return the upper-left and lower-right corners
        # (these are used when specifying canvas objects like an oval):
        return (self.x - self.r/2.0, self.y + self.r/2.0,
                self.x + self.r/2.0, self.y - self.r/2.0)
    
    # move to a new point (x,y) in physical coordinates:
    def abs_move(self, x, y):
        self.x = x;  self.y = y
        # canvas.move makes a relative move (dx,dy)
        # canvas.coords makes an absolute move to new (x,y)
        c = C.physical2canvas4(self.get_corners())
        self.canvas.coords(self.id, c[0], c[1], c[2], c[3])

    # make a relative move (dx,dy) in physical coordinates
    def rel_move(self, dx, dy):
        self.x = self.x + dx;  self.y = self.y + dy
        dx = C.scale(dx);  dy = C.scale(dy)
        # relative move in canvas coords will be (dx,-dy):
        self.canvas.move(self.id, dx, -dy)

    def __str__(self):
        return 'object %d: physical center=(%g,%g) radius=%g' %\
               (self.id,self.x,self.y,self.r)
    
    
class PlanetarySystem:
    
    def __init__(self, parent, model=MathModel1(),
                 w=400, h=400  # canvas size
                 ):

        self.parent = parent
        self.model = model
        self.frame = Frame(parent, borderwidth=2)
        self.frame.pack()
        C.set_coordinate_system(w, h, w/2, h/2, 1.0)
        C.print_coordinate_system()
        self.canvas = Canvas(self.frame, width=w, height=h,
                             background='white')
        self.canvas.pack()
        self.parent.bind('<q>', self.quit)

	# self.planets: dictionary of sun and planet,
        # indexed by their canvas IDs
        self.planets = {}
        # create sun:
        sun = Planet(x=0, y=0, radius=60, color='orange',
                     canvas=self.canvas)
        self.planets[sun.id] = sun
        self.sun_id = sun.id  # to distinguish sun from other planets later

        # create planet:
        planet = Planet(x=0.2, y=0.3, radius=30, color='green',
                        canvas=self.canvas)
        self.planets[planet.id] = planet

        print sun, planet

    def animate(self):
        for id in self.planets.keys():
            if id != self.sun_id: planet_id = id

        # initialize data structures in model:
        self.model.x_center = self.planets[self.sun_id].x
        self.model.y_center = self.planets[self.sun_id].y
        self.model.x = self.planets[planet_id].x
        self.model.y = self.planets[planet_id].y
        # default values of omega and nsteps_per_round is ok
        self.model.init()

        # unnecessary (just a consistency test that abs_move does not move):
        self.planets[planet_id].abs_move(self.model.x, self.model.y)

        while self.model.time() < 500:

            self.model.advance()
            (x,y) = self.model.get_current_state()

            # draw line segment from previous position:
            (x_prev,y_prev) = self.model.get_previous_state()
            c = C.physical2canvas4((x_prev,y_prev,x,y))
            self.canvas.create_line(c[0],c[1],c[2],c[3])

            # no sign. difference in the speed of rel_move vs. abs_move:
            # dx = x - x_prev; dy = y - y_prev
            # self.planets[planet_id].rel_move(dx,dy)

            self.planets[planet_id].abs_move(x,y) # move planet

            # control speed of item:
            self.canvas.after(50)  # delay in milliseconds

            # required for the movement to be updated in each
            # iteration (otherwise the graphics are updated after
            # the function is completed):
            self.canvas.update()

    def quit(self, event):  self.parent.quit()

if __name__ == '__main__':
    root = Tk()
    w = PlanetarySystem(root)
    w.animate()
    root.mainloop()
