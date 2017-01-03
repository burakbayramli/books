#!/usr/bin/env python
from Tkinter import *

# the first canvas encounter:
    
root = Tk()
c = Canvas(root,width=400, height=400)
c.pack()
c.create_oval(100, 100, 200, 200, fill='red', outline='blue')
c.create_text(100, 100, text='(100,100)')
c.create_text(200, 200, text='(200,200)')
c.create_line(100,100, 100,200, 200,200, 200,100, 100,100)
c.create_text(150, 250, text='bounding box')
c.create_line(150,250, 50,200, 100,150, arrow='last', smooth=True)
# bind 'p' on the keyboard to a PostScript dump:
def dump_PS(event):
    print 'making postscript...'
    c.postscript(file='myfirstcanvas.ps')
root.bind('<p>', dump_PS)
def quit(event):
    root.quit()
root.bind('<q>', quit)
c.mainloop()

