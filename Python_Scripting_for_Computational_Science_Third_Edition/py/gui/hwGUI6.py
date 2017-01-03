#!/usr/bin/env python
from Tkinter import *
import math

root = Tk()              # root (main) window
top = Frame(root)        # create frame
top.pack(side='top')     # pack frame in main window

# create frame to hold the first widget row:
hwframe = Frame(top)
# this frame (row) is packed from top to bottom (in the top frame):
hwframe.pack(side='top')
# set the Hello World font:
font = 'times 18 bold'
# alternatives:
#font = ('Times', 18, 'bold')
#font = '-adobe-times-bold-r-normal-*-18-*-*-*-*-*-*-*'
# create label in the frame:
hwtext = Label(hwframe, text='Hello, World!', font=font)
hwtext.pack(side='top', pady=20)

# create frame to hold the middle row of widgets:
rframe = Frame(top)
# this frame (row) is packed from top to bottom:
rframe.pack(side='top', padx=10, pady=20)
# create label and entry in the frame and pack from left:
r_label = Label(rframe, text='The sine of')
r_label.pack(side='left')
r = StringVar() # variable to be attached to r_entry
r.set('1.2')    # default value
r_entry = Entry(rframe, width=6, textvariable=r)
r_entry.pack(side='left')

s = StringVar() # variable to be attached to s_label
def comp_s(event):
    s.set('%g' % math.sin(float(r.get()))) # construct string

r_entry.bind('<Return>', comp_s)

compute = Label(rframe, text=' equals ')
compute.pack(side='left')

s_label = Label(rframe, textvariable=s, width=12)
s_label.pack(side='left')

# finally, make a quit button and a binding of q to quit
# quit needs one event argument when called from a keyboard
# binding, while no argument when called from a button
# this is solved using default arguments:

def quit(event=None):  # can take one or no arguments
    root.destroy()

quit_button = Button(top, text='Goodbye, GUI World!', command=quit)
quit_button.pack(side='top',pady=5)

root.bind('<q>', quit)

root.mainloop()
