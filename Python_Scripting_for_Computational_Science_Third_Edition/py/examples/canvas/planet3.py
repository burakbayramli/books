#!/usr/bin/env python
# as planet1.py, but here we add functionality for dragging the
# planets around in the canvas

import Tkinter, Pmw
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

    # make a relative move when dragging the mouse:
    def mouse_move(self, xc, yc, xc_prev, yc_prev):
        #print 'move %d from (%d,%d) to (%d,%d) = (%d,%d)' % (self.id,xc_prev,yc_prev,xc,yc,xc-xc_prev,yc-yc_prev),
        # move object to new mouse position (xc,yc) in canvas coords:
        self.canvas.move(self.id, xc-xc_prev, yc-yc_prev)
        # update the planet's physical coordinates:
        c = self.canvas.coords(self.id)  # grab new canvas coords
        corners = C.canvas2physical4(c)  # transform to physical coords
        self.x, self.y = self.get_center(corners)
        # could check for consistency: self.abs_move(self.x,self.y)
        #print '=(%g,%g)' % (self.x,self.y)

    # compute center based on upper-left and lower-right corner
    # coordinates in physical coordinate system:
    def get_center(self, corners):
        return (corners[0] + self.r/2.0, corners[1] - self.r/2.0)

class PlanetarySystem:
    
    def __init__(self, parent, model=MathModel1(), w=400, h=400):

        self.parent = parent
        self.model = model
        self.frame = Tkinter.Frame(parent, borderwidth=2)
        self.frame.pack(fill='both', expand=True)
        C.set_coordinate_system(w, h, w/2, h/2, 1.0)
        C.print_coordinate_system()
        self.canvas = Pmw.ScrolledCanvas(self.frame,
                                         labelpos='n',
                                         label_text='Canvas demo',
                                         usehullsize=1,
                                         hull_width=w, hull_height=h)
        self.canvas.pack(fill='both', expand=True)
        self.parent.bind('<q>', self.quit)
        self.canvas.component('canvas').bind('<Button-1>',  self.mark)
        self.canvas.component('canvas').bind('<B1-Motion>', self.drag)
        self.canvas.component('canvas').configure(background='white')

        self.planets = {}  # sun and planets
        # create sun:
        sun = Planet(x=0, y=0, radius=60, color='orange',
                     canvas=self.canvas)
        # store in dictionary with the planet's id (int) as index:
        self.planets[sun.id] = sun
        self.sun_id = sun.id  # to distinguish sun from other planets later

        # create planet:
        planet = Planet(x=0.2, y=0.3, radius=30, color='green', 
                        canvas=self.canvas)
        self.planets[planet.id] = planet

        print sun, planet
        self.stop_animation = 0

        self.speed = Tkinter.DoubleVar(); self.speed.set(1);
        speed_scale = Tkinter.Scale(self.frame, orient='horizontal',
          from_=0, to=1, tickinterval=0.5, resolution=0.01, 
          label='animation speed', length=300,
          variable=self.speed)
        speed_scale.pack(side='top')

        button_row = Tkinter.Frame(self.frame, borderwidth=2)
        button_row.pack(side='top')
        b = Tkinter.Button(button_row, text='start animatation',
                           command=self.animate)
        b.pack(side='left')
        b = Tkinter.Button(button_row, text='stop animation',
                           command=self.stop)
        b.pack(side='right')

        # do _not_ set the scroll region of the canvas to include all
        # created items: self.canvas.resizescrollregion()
        # (try it if you're not convinced...)

    def stop(self):
        self.stop_animation = True
        
    def animate(self):
        self.stop_animation = False
        for id in self.planets.keys():  # find planet's (or planets') ID(s)
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

        while self.model.time() < 5 and not self.stop_animation:

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
            self.canvas.after(int((1-self.speed.get())*1000)) # delay in ms

            # required for the movement to be updated in each
            # iteration (otherwise the graphics are updated after
            # the function is completed):
            self.canvas.update()

            
    def mark(self, event):
        w = event.widget
        # find the canvas coords from screen coords:
        xc = w.canvasx(event.x);  yc = w.canvasx(event.y)
        # find the id of the object being pointed to:
        self.clicked_item = self.canvas.find_withtag('current')[0]
        # store the (xc,yc) coordinates of the clicked item:
        self.clicked_item_xc_prev = xc; self.clicked_item_yc_prev = yc

    def drag(self, event):
        w = event.widget  # could test that this is self.canvas...
        xc = w.canvasx(event.x); yc = w.canvasx(event.y)
        self.planets[self.clicked_item].mouse_move \
          (xc, yc, self.clicked_item_xc_prev, self.clicked_item_yc_prev)
        # update previous pos to present one (init for next drag call):
        self.clicked_item_xc_prev = xc; self.clicked_item_yc_prev = yc

    def quit(self, event):  self.parent.quit()


if __name__ == '__main__':
    root = Tkinter.Tk()
    Pmw.initialise(root, fontScheme='pmw1')
    root.title('Demo of a canvas widget')
    w = PlanetarySystem(root)
    root.mainloop()
