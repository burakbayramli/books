#!/usr/bin/env python
from Tkinter import *
root = Tk()
root.configure(background='gray')
row = 0
for color in ('red', 'orange', 'yellow', 'blue', 'green',
              'brown', 'purple', 'gray', 'pink'):
    l = Label(root, text=color, background='white')
    l.grid(row=row, column=0)
    f = Frame(root, background=color, width=100, height=2)
    f.grid(row=row, column=1)
    row = row + 1
root.mainloop()
