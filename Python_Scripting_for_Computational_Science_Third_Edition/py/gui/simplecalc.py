#!/usr/bin/env python
"""implement a very simple calculator, just evaluating Python math"""
from Tkinter import *
from math import *

root = Tk()
top = Frame(root); top.pack()
Label(top, text='Define f(x):').pack(side='left')
# use get and configure to interact with the texts
f_entry = Entry(top, width=12)
f_entry.pack(side='left')
f_entry.insert('end', 'x')
        
Label(top, text='  x =').pack(side='left')
x_entry = Entry(top, width=6)
x_entry.pack(side='left')
x_entry.insert('end', '0')

s_label = Label(top, width=9) # holds the func value

def calc(event=None):
    f_txt = f_entry.get() # get function expression as string
    x = float(x_entry.get())  # define x
    res = eval(f_txt)    # the magic line calculating f(x)
    s_label.configure(text='%g' % res)   # display f(x) value
    
x_entry.bind('<Return>', calc)
Button(top, text='  f = ', relief='flat',
       command=calc).pack(side='left')
s_label.pack(side='left')

def quit(event=None): root.quit()
root.bind('<q>', quit)
root.mainloop()
