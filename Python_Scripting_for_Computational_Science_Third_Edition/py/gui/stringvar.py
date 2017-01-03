#!/usr/bin/env python
from Tkinter import *
root = Tk()
r = StringVar()
Entry(root, textvariable=r).pack()
Label(root, textvariable=r).pack()
root.mainloop()
