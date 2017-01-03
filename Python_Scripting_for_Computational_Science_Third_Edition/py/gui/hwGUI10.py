#!/usr/bin/env python
"""Class version of hwGUI9.py."""

from Tkinter import *
import math

class HelloWorld:
    def __init__(self, parent):
        self.master = parent   # store the parent
        top = Frame(parent)    # frame for all class widgets
        top.pack(side='top')   # pack frame in parent's window

        # create frame to hold the first widget row:
        hwframe = Frame(top)
        # this frame (row) is packed from top to bottom:
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

        self.r = StringVar() # variable to be attached to r_entry
        self.r.set('1.2')    # default value
        r_entry = Entry(rframe, width=6, textvariable=self.r)
        r_entry.pack(side='left')
        r_entry.bind('<Return>', self.comp_s)

        compute = Button(rframe, text=' equals ',
                         command=self.comp_s, relief='flat')
        compute.pack(side='left')

        self.s = StringVar() # variable to be attached to s_label
        s_label = Label(rframe, textvariable=self.s, width=12)
        s_label.pack(side='left')

        # finally, make a quit button:
        quit_button = Button(top, text='Goodbye, GUI World!',
                             command=self.quit,
                             background='yellow',foreground='blue')
        quit_button.pack(side='top', pady=5, fill='x')
        self.master.bind('<q>', self.quit)

    def quit(self, event=None):
        self.master.quit()

    def comp_s(self, event=None):
        self.s.set('%g' % math.sin(float(self.r.get())))


root = Tk()               # root (main) window
hello = HelloWorld(root)
root.mainloop()
