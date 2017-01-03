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
# create label in the frame:
font = 'times 18 bold'
hwtext = Label(hwframe, text='Hello, World!', font=font)
hwtext.pack(side='top', pady=20)

# create frame to hold the middle row of widgets:
rframe = Frame(top)
# this frame (row) is packed from top to bottom:
rframe.pack(side='top', padx=10, pady=20)

# create label and entry in the frame and pack from left:
r_label = Label(rframe, text='The sine of')
r_label.pack(side='left')

# do not tie a variable to the entry, use get and insert instead
r_entry = Entry(rframe, width=6)
r_entry.pack(side='left')
r_entry.insert('end', '1.2') # insert default value

def comp_s(event=None):
    r = float(r_entry.get())
    s = math.sin(r)
    s_label.configure(text='%g' % s)

r_entry.bind('<Return>', comp_s)

compute = Button(rframe, text=' equals ', command=comp_s, relief='flat')
compute.pack(side='left')

# do not tie a variable to the label, use configure in comp_s:
s_label = Label(rframe, width=12)
s_label.pack(side='left')

# finally, make a quit button:
def quit(event=None):
    root.destroy()
quit_button = Button(top, text='Goodbye, GUI World!', command=quit,
                     background='yellow', foreground='blue')
quit_button.pack(side='top', pady=5, fill='x')
root.bind('<q>', quit)

root.mainloop()
