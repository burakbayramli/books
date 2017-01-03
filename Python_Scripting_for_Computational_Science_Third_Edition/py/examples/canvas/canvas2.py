#!/usr/bin/env python
"""The first canvas encounter, using physical coordinates."""
from Tkinter import *
from scitools.CanvasCoords import CanvasCoords
C = CanvasCoords()
root = Tk()
c = Canvas(root,width=400, height=400)
c.pack()
# let physical (x,y) be at (200,200) and let the x range be 2:
C.set_coordinate_system(400,400, 200,200, 2.0)
cc = C.physical2canvas4((0.2,0.2,0.6,0.6))
c.create_oval(cc[0], cc[1], cc[2], cc[3], fill='red',outline='blue')
c1, c2 = C.physical2canvas(0.2, 0.2)
c.create_text(c1, c2, text='(0.2, 0.2)')
c1, c2 = C.physical2canvas(0.6, 0.6)
c.create_text(c1, c2, text='(0.6, 0.6)')
c.create_line(C.cx(0.2), C.cy(0.2),
              C.cx(0.6), C.cy(0.2),
              C.cx(0.6), C.cy(0.6),
              C.cx(0.2), C.cy(0.6),
              C.cx(0.2), C.cy(0.2))

# bind 'p' on the keyboard to a PostScript dump:
def dump_PS(event):
    print 'making postscript...'
    c.postscript(file='mysecondcanvas.ps')
root.bind('<p>', dump_PS)
def quit(event):
    root.quit()
root.bind('<q>', quit)
c.mainloop()
