#!/usr/bin/env python
from Tkinter import *
import math

root = Tk()              # root (main) window
top = Frame(root)        # create frame
top.pack(side='top')     # pack frame in main window

# use grid to place widgets in 3x4 cells:

font = 'times 18 bold'
hwtext = Label(top, text='Hello, World!', font=font)
hwtext.grid(row=0, column=0, columnspan=4, pady=20)

r_label = Label(top, text='The sine of')
r_label.grid(row=1, column=0)

r = StringVar() # variable to be attached to r_entry
r.set('1.2')    # default value
r_entry = Entry(top, width=6, textvariable=r)
r_entry.grid(row=1, column=1)

s = StringVar() # variable to be attached to s_label
def comp_s(event=None):
    s.set('%g' % math.sin(float(r.get()))) # construct string

r_entry.bind('<Return>', comp_s)

compute = Button(top, text=' equals ', command=comp_s, relief='flat')
compute.grid(row=1, column=2)

s_label = Label(top, textvariable=s, width=12)
s_label.grid(row=1, column=3)

# finally, make a quit button:
def quit(event=None):
    root.destroy()
quit_button = Button(top, text='Goodbye, GUI World!', command=quit,
                     background='yellow', foreground='blue')
quit_button.grid(row=2, column=0, columnspan=4, pady=5, sticky='ew')
root.bind('<q>', quit)

root.mainloop()
