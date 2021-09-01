# Analog clock
from tkinter import *                                 # import Tkinter module
from math import *
from time import *

def Clock0(w, nx, ny):                                  # clock draw function
   x0 = nx/2; lx = 9*nx/20              # center and half-width of clock face
   y0 = ny/2; ly = 9*ny/20
   r0 = 0.9 * min(lx,ly)                # distance of hour labels from center
   r1 = 0.6 * min(lx,ly)                                # length of hour hand
   r2 = 0.8 * min(lx,ly)                              # length of minute hand

   w.create_oval(x0-lx, y0-ly, x0+lx, y0+ly, width=2)            # clock face
   for i in range(1,13):                               # label the clock face
      phi = pi/6 * i                              # angular position of label
      x = x0 + r0 * sin(phi)                    # Cartesian position of label
      y = y0 - r0 * cos(phi)
      w.create_text(x, y, text=str(i))                           # hour label

   t = localtime()                                             # current time
   t_s = t[5]                                                       # seconds
   t_m = t[4] + t_s/60                                              # minutes
   t_h = t[3] % 12 + t_m/60                                    # hours [0,12]

   phi = pi/6 * t_h                                         # hour hand angle
   x = x0 + r1 * sin(phi)                             # position of arrowhead
   y = y0 - r1 * cos(phi)                                    # draw hour hand
   w.create_line(x0, y0, x, y, arrow=LAST, fill="red", width=3)

   phi = pi/30 * t_m                                      # minute hand angle
   x = x0 + r2 * sin(phi)                             # position of arrowhead
   y = y0 - r2 * cos(phi)                                  # draw minute hand
   w.create_line(x0, y0, x, y, arrow=LAST, fill="blue", width=2)

   phi = pi/30 * t_s                                      # second hand angle
   x = x0 + r2 * sin(phi)                             # position of arrowhead
   y = y0 - r2 * cos(phi)
   w.create_line(x0, y0 , x, y, arrow=LAST)                # draw second hand

# main

root = Tk()                                           # create Tk root widget
root.title("Python clock")

nx = 300; ny = 300                                              # canvas size
w = Canvas(root, width=nx, height=ny, bg = "white")           # create canvas
w.pack()                                                # make canvas visible

while (1):                                                    # infinite loop
   w.delete(ALL)                                              # delete canvas
   Clock0(w, nx, ny)                                             # draw clock
   w.update()                                                 # update canvas

root.mainloop()                                    # enter Tkinter event loop
