#!/usr/bin/env python
from Tkinter import *
import math, random, numpy, copy, time

# think of combining with C: move particles in C, share occupied
# array with Py(NumPy) and visualize occupied now and then
# (difficult to find the non-drawn parts? could just visualize it
# 10 times and then redraw it every time, yes, that's ok)
# print nstep now and then

class DLA:
    def coor(self, x, y):
        # return Canvas coordinates when (x,y) are 'natural' coordinates:
        return (x, self.frame_height - y)
    def coor4(self, x1, y1, x2, y2):
        return (x1, self.frame_height - y1, x2, self.frame_height - y2)
    
    def __init__(self,
                 parent,
                 nparticles = 4,
                 update = 1,
                 nsteps = 10,
                 particle_frame = 0,
                 seed1 = 10, 
                 w=400, h=400):

        random.seed(seed1)
        print 'first random number=', random.random()
        self.master = parent
        frame = Frame(parent, borderwidth=2)
        frame.pack()
        self.frame_width  = w
        self.frame_height = h
        print 'w=',self.frame_width,'h=',self.frame_height
        self.canvas = c = Canvas(frame,
                                 width=self.frame_width,
                                 height=self.frame_height,
                                 background='black')
        c.pack()

        self.nparticles = nparticles
        self.update = update  # continuous movement or not
        self.nsteps = nsteps
        self.occupied = numpy.zeros((w,h), int)
        self.finished = 0
        self.master.bind('q', self.quit)
        self.particle_color = 'blue'
        self.particle_frame = particle_frame
        
        self.freespace = 10+2*(particle_frame-1)
        # seed:
        self.seed = self.coor(self.frame_width/2, self.frame_height/2)
        p = self.createParticle(self.seed)
        self.occupied[p[1],p[2]] = 1  # mark occupied array

        # inner frame:
        self.ifxmin = self.ifxmax = p[1]
        self.ifymin = self.ifymax = p[2]
        self.changeBoundingBox(p[1],p[2])
        xc1,yc1,xc2,yc2 = self.coor4(self.ifxmin,self.ifymin,
                                     self.ifxmax,self.ifymax)
        c.create_rectangle(xc1,yc1,xc2,yc2,tags='boundingbox',
                           outline='green')

        self.particles = []
        for p in range(self.nparticles):
            self.particles.append(
                self.createParticle(self.randomPointOnInnerFrame()))

        c0 = time.clock()
        color = 1; forward=1
        for self.step in range(self.nsteps):
            if self.finished: break
            
            if self.step % 50 == 0:
                color = color % 255 + 1  # 2,3,4,...,255
                if forward:
                    # blue to green:
                    self.particle_color = '#%02x%02x%02x' % (0, color, 255)
                else:
                    # reverse back to blue:
                    self.particle_color = '#%02x%02x%02x' % (0, 256-color, 255)

                if color == 255:
                    forward = not forward
                
            for p in self.particles:
                self.move(p)

                # if continuous update, update for each particle move:
                #if self.update: self.master.update()

            # update here is more efficient:
            if self.update: self.master.update()

            if self.finished:
                break
        # finished!
        cpu = time.clock() - c0
        print 'CPU-time of this simulation:', cpu, 'seconds'
        print 'dla.ps contains a PostScript plot of the DLA'
        # in the PostScript plot, the extent of the particles must
        # be non-zero, set them to 1 (in canvas coordinate measurement):
        items = self.canvas.find_all()
        print len(items), 'particles in this simulation'
        for item in items:  # run through all items
            coor = self.canvas.coords(item) # grab particle coordinates
            coor[2] = coor[2] + 1;  coor[3] = coor[3] + 1
            self.canvas.coords(item, coor[0], coor[1], coor[2], coor[3])
        try:     self.canvas.delete('boundingbox')
        except:  pass
        self.canvas.postscript(file='dla.ps')
             
    def move(self, particle):
        # particle: list where [0] is canvas representation,
        # [1] is x coordinate and [2] is y coordinate,
        
        # with probability 0.5 move horizontally,
        # with new probablility 0.5 move west or east
        # same for vertical movement
        r1 = random.random(); r2 = random.random()
        dx = 0;  dy = 0;
        if r1 < 0.5:      # move horizontally:
            if r2 < 0.5:  dx = -1 # west
            else:         dx = +1 # east
        else:             # move vertically:
            if r2 < 0.5:  dy = -1 # south
            else:         dy = +1 # north
        # update particle's coordinates:
        particle[1] = particle[1] + dx
        particle[2] = particle[2] + dy

        if self.movedOut(particle):
            return
        
        # update graphics?
        if self.update:
            self.canvas.move(particle[0], dx, -dy) # -dy in canvas coord
           #self.debug(particle, 'move')
            
        self.isStuck(particle) # if stuck, create new particle

    def debug(self, particle, msg=''):
        xc,yc,xc1,yc1=self.canvas.coords(particle[0])
        xc2,yc2=self.coor(xc,yc)
        x = particle[1]; y = particle[2]
        if x-xc2 != 0 or y-yc2 != 0:
            print '%s: particle %d(%d,%d) vs coords=(%d,%d) diff=(%d,%d)' % (msg, particle[0], xc2,yc2,x,y,x-xc2,y-yc2)

    def movedOut(self, particle):
        h = 5
        #if particle[1] < 0 or particle[1] > self.frame_width or \
        #   particle[2] < 0 or particle[2] > self.frame_height:
        if particle[1] < self.ifxmin-h or particle[1] > self.ifxmax+h or \
           particle[2] < self.ifymin-h or particle[2] > self.ifymax+h or \
           particle[1] < 0 or particle[1] > self.frame_width or \
           particle[2] < 0 or particle[2] > self.frame_height:
            p = self.createParticle(self.randomPointOnInnerFrame())
           #print '(%d,%d) is out - created a new particle at(%d,%d)' % (particle[1],particle[2],p[1],p[2])
            # remove current particle:
            self.canvas.delete(particle[0])
            particle[0:2] = copy.deepcopy(p)  # EXPLAIN!!!!!!!
           #self.debug(particle, 'moved out, createParticle')
            return 1
        else:
            return 0

    # cannot change particle inside functions with effect outside
    # (which is of fundamental importance here)
    # only particle[0:2] = copy.deepcopy(p) works!
    
    def isStuck(self, particle):
        stuck = 0
        # self.debug(particle, 'check if stuck')
        for i in range(-1,2,1):
            for j in range(-1,2,1):
                if i != 0 or j != 0:
                    try:
                        if self.occupied[particle[1]+i,particle[2]+j]:
                            stuck = 1
                            break
                    except:
                        pass
        if stuck:
            x = particle[1]; y = particle[2]
            self.occupied[x,y] = 1
            # self.debug(particle, 'stuck:')

            self.changeBoundingBox(x,y)
            self.testBB(x,y)
            
            # create a new particle:
            p = self.createParticle(self.randomPointOnInnerFrame())
            #s = '(%d,%d) is stuck - created a new particle' % (particle[1],particle[2])
            #particle[0:2] = p[0:2]  # does not work
            # particle[0:2]: in-place modification(a la pass by ref.)
            # copy.deepcopy: take a true copy of all elements
            particle[0:2] = copy.deepcopy(p)
            #print s, 'at(%d,%d)' % (particle[1],particle[2])
            #self.debug(particle, 'after createParticle')
            return 1
        else:
            return 0

            
    def createParticle(self, xy_point):
        x, y = xy_point   # tuple
        xc, yc = self.coor(x,y)
        if self.particle_frame == 0:
            p = self.canvas.create_rectangle(xc, yc, xc, yc,
                                             fill=self.particle_color,
                                             outline='', width=0)
        else:
            p = self.canvas.create_rectangle(xc, yc, xc, yc,
                                             fill=self.particle_color,
                                             outline=self.particle_color,
                                             width=self.particle_frame)
        return [p,x,y]
            
    def randomPointOnInnerFrame(self):
        r = random.random()
        w = self.ifxmax - self.ifxmin
        h = self.ifymax - self.ifymin
        if r < 0.25:   # east side:
            x = self.ifxmax;    y = self.ifymin + int((r/0.25)*h)
        elif r < 0.5:  # north side:
            x = self.ifxmin + int(((r-0.25)/0.5)*w);  y = self.ifymax
        elif r < 0.75: # west side:
            x = self.ifxmin;  y = self.ifymin + int(((r-0.5)/0.25)*h)
        else:          # south side:
            x = self.ifxmin + int(((r-0.75)/0.25)*w); y = self.ifymin
        #print 'inner frame: [%d,%d]x[%d,%d] new particle (%d,%d)' % (self.ifxmin,self.ifxmax,self.ifymin,self.ifymax,x,y)
        return (x,y)

    def changeBoundingBox(self, x, y):
        changed = 0
        dx = x - self.freespace - self.ifxmin
        if dx < 0:
            self.ifxmin = self.ifxmin - abs(dx); changed = 1
        dx = self.ifxmax -(x + self.freespace)
        if dx < 0:
            self.ifxmax = self.ifxmax + abs(dx); changed = 1
        dy = y - self.freespace - self.ifymin
        if dy < 0:
            self.ifymin = self.ifymin - abs(dy); changed = 1
        dy = self.ifymax -(y + self.freespace)
        if dy < 0:
            self.ifymax = self.ifymax + abs(dy); changed = 1
        # remove the old bounding box and draw a new
        if changed:
            try:
                self.canvas.delete('boundingbox')
            except:
                pass
            #print 'BB: [%d,%d]-[%d,%d]' % (self.ifxmin,self.ifymin,self.ifxmax,self.ifymax)
            xc1,yc1,xc2,yc2 = self.coor4(self.ifxmin,self.ifymin,
                                         self.ifxmax,self.ifymax)
            if self.update:
                self.canvas.create_rectangle \
               (xc1,yc1,xc2,yc2,tags='boundingbox',outline='green')
        # finished?
        if self.ifxmin <= 0 or self.ifxmax >= self.frame_width or \
           self.ifymin <= 0 or self.ifymax >= self.frame_height:
            self.finished = 1

    def testBB(self, x, y):
        if x < self.ifxmin or x > self.ifxmax or \
           y < self.ifymin or y > self.ifymax:
            print '(%d,%d) not in BB [%d,%d]-[%d,%d]' % (x,y,self.ifxmin,self.ifymin,self.ifxmax,self.ifymax)

    def quit(self, event):  self.master.quit()
        
if __name__ == '__main__':
    root = Tk()
    if len(sys.argv) < 1:
        print 'Usage:',sys.argv[0],'[-nsteps 10000 -pframe 0 -nparticles 20 -w 300 -h 300 -seed 1 6 4]'
        sys.exit(1)
        
    # default values:
    update = 1
    nsteps = 1000
    particle_frame = 0
    nparticles = 20
    width = 400
    height = 400
    seed1 = 10

    arg_counter = 1
    while arg_counter < len(sys.argv):
        option = sys.argv[arg_counter]
        if option == '-nsteps':
            arg_counter = arg_counter + 1
            nsteps = int(sys.argv[arg_counter])
        elif option == '-w':
            arg_counter = arg_counter + 1
            width = int(sys.argv[arg_counter])
        elif option == '-h':
            arg_counter = arg_counter + 1
            height = int(sys.argv[arg_counter])
        elif option == '-pframe':
            arg_counter = arg_counter + 1
            particle_frame = int(sys.argv[arg_counter])
        elif option == '-nparticles':
            arg_counter = arg_counter + 1
            nparticles = int(sys.argv[arg_counter])
        elif option == '-seed':
            arg_counter = arg_counter + 1
            seed1 = int(sys.argv[arg_counter])
        else:
            print sys.argv[0],': invalid option',option
            sys.exit(1)
        arg_counter = arg_counter + 1

    dla = DLA(root, update=update, nsteps=nsteps,
              particle_frame=particle_frame,
              nparticles=nparticles,
              w=width,h=height, seed1=seed1)
    root.mainloop()






