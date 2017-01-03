#!/usr/bin/env python

import Tkinter, Pmw 
import math, whrandom, Numeric, copy, time 
from scitools.CanvasCoords import CanvasCoords
C = CanvasCoords()  # transformation object 
# default values 
C.set_coordinate_system(400,800,200,200) 
 
from vtk import *
import vtkTkRenderWidget
from model1 import * 
 
class Planet: 
    def __init__(self, name, source=None,
                 mapper=None, x=0, y=0, z=0, with_disk=None): 
        self.source, self.mapper = source, mapper 
        self.x, self.y, self.z = x, y, z 
 
        if source == None: 
            self.source = vtkConeSource() 
 
        if mapper == None: 
            self.mapper = vtkMapper() 
 
        self.mapper.SetInput(self.source.GetOutput()) 
         
        self.actor = vtkActor() 
        self.actor.SetMapper(self.mapper) 
 
        self.move(self.x, self.y, self.z) 
 
        if with_disk: 
            # create disk showing orbit 
            disk_source = vtkDiskSource() 
            disk_source.SetCircumferentialResolution( 20 ) 
            disk_source.SetInnerRadius(0.35) 
            disk_source.SetOuterRadius(0.36) 
 
            ## if the orbit of the planet is not aligned with 
            ## the sun's equator, use the transformation object to 
            ## correct the orbit. 
            # transform_filter = vtkTransformPolyDataFilter() 
            # transformer = vtkTransform() 
            # transformer.RotateX(0) 
            # transform_filter.SetInput(disk_source.GetOutput()) 
            # transform_filter.SetTransform(transformer) 
             
            disk_mapper = vtkPolyDataMapper() 
            # disk_mapper.SetInput(transform_filter.GetOutput()) 
            disk_mapper.SetInput(disk_source.GetOutput()) 
            self.disk_actor = vtkActor() 
            self.disk_actor.SetMapper(disk_mapper) 
         
    def move(self, x, y, z): 
        self.actor.SetPosition(x, y, z) 
 
    def get_position(self): 
        return (self.x, self.y, self.z) 
 
 
class PlanetarySystem: 
    def create_sun(self): 
        """ 
        create a yellow sun, with coordinates (0,0,0) 
        """ 
        sun_source = vtkSphereSource() 
        sun_source.SetRadius( 0.2 ) 
        sun_source.SetThetaResolution( 20 ) 
        sun_source.SetPhiResolution( 20 ) 
         
        sun_mapper = vtkPolyDataMapper() 
 
        self.planets['sun'] = Planet('sun', sun_source, sun_mapper) 
        self.planets['sun'].actor.GetProperty().SetColor(1, 1, 0) 
        self.renderer.AddActor( self.planets['sun'].actor ) 
 
    def create_planet(self, name, x, y, z): 
        """ 
        create a planet (with disk) at specified coordinates 
        """ 
        if self.__planet_source == None and self.__planet_mapper == None: 
            self.__planet_source = vtkSphereSource() 
            self.__planet_source.SetRadius( 0.1 ) 
            self.__planet_source.SetThetaResolution( 20 ) 
            self.__planet_source.SetPhiResolution( 20 ) 
 
            self.__planet_mapper = vtkPolyDataMapper() 
 
        self.planets[name] = Planet(name, self.__planet_source, 
                                    self.__planet_mapper, 
                                    x, y, z, with_disk=1) 
        self.renderer.AddActor( self.planets[name].actor ) 
        self.renderer.AddActor( self.planets[name].disk_actor ) 
                 
    def __init__(self, parent, model=MathModel1(), w=400, h=400): 
        self.parent = parent 
        self.model = model 
        self.frame = Tkinter.Frame(parent, borderwidth=2, colormap='new') 
        self.parent.bind('<q>', self.quit) 
 
        self.__planet_source = self.__planet_mapper = None 
 
        # create renderer and renderwindow 
        self.renderer = vtkRenderer() 
        from vtk.tk.vtkTkRenderWindowInteractor import \
             vtkTkRenderWindowInteractor
        self.window_w = vtkTkRenderWindowInteractor(master)
        self.window_w.pack( side = 'top', expand = 1, fill = 'both' ) 
        self.window = self.window_w.GetRenderWindow() 
        self.window.AddRenderer( self.renderer ) 
 
        self.planets = {}  # sun and planets 
 
        # create sun: 
        self.create_sun() 
 
        # create planet 1: 
        self.create_planet('earth', 0.2, 0.3, 0) 
                 
        # gooey: 
        self.delay = Tkinter.DoubleVar(); self.delay.set(0.01); 
        delay_scale = Tkinter.Scale(self.frame, orient='horizontal', 
          from_=0.01, to=1, tickinterval=100, resolution=0.01,  
          label='slow-down of motion', length=300, 
          variable=self.delay) 
        delay_scale.pack(side='top') 
 
        button_row = Tkinter.Frame(self.frame, borderwidth=2, colormap='new') 
        button_row.pack(side='top', fill='both', expand=True) 
        b = Tkinter.Button(button_row, text="start animatation", 
                           command=self.animate) 
        b.pack(side='left',fill='x',expand=True) 
        b = Tkinter.Button(button_row, text="stop animation", 
                           command=self.stop) 
        b.pack(side='right',fill='x',expand=True) 
 
        self.frame.pack(fill='both', expand=True) 
        self.window.Render() 
 
        self.__animate_thread_active = 0 
 
    def stop(self): 
        self.stop_animation = True 
 
    def animate(self): 
        import time 
         
        self.stop_animation = False
 
        # initialize data structures in model:
        self.model.x_center = self.planets['sun'].get_position()[0]
        self.model.y_center = self.planets['sun'].get_position()[1]
        self.model.x = self.planets['earth'].get_position()[0]
        self.model.y = self.planets['earth'].get_position()[1]
        self.nsteps_per_round = 40
        self.model.init()
                                  
        while self.model.time() < 5 and not self.stop_animation: 
 
            self.model.advance() 
            (x,y) = self.model.get_current_state()

            self.planets['earth'].move(x,y,0)  
            self.window.Render() 
             
            # we need to sleep to get the thread to yield (pause execution so 
            # we can recieve input from the user) 
            time.sleep(self.delay.get()) 
            self.parent.update() 
         
    def quit(self, event):  self.parent.quit() 
 
if __name__ == '__main__': 
    root = Tkinter.Tk() 
    Pmw.initialise(root, fontScheme='pmw1') 
    root.title('Demo of a canvas widget') 
    w = PlanetarySystem(root) 
    root.mainloop() 
