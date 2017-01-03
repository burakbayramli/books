#!/usr/bin/env python
from Tkinter import *
from math import *


class FunctionEvaluator1:
    def __init__(self, parent):
        frame = Frame(parent); frame.pack(padx=20, pady=30)
        self.master = parent
        Label(frame, text='Define f(x):').pack(side='left')

        # use get and configure to interact with the texts
        f_entry = Entry(frame, width=12)
        f_entry.pack(side='left')
        f_entry.insert('end', 'x')
        
        Label(frame, text='  x =').pack(side='left')

        x_entry = Entry(frame, width=6)
        x_entry.pack(side='left')
        x_entry.insert('end', '0')

        s_label = Label(frame, width=9) # holds the func value

        x_entry.bind('<Return>', 
                  lambda event, f=f_entry, x=x_entry,
                          label=s_label, func=self.calc:
                          func(event, f, x, label))

        compute = Button(frame, text='  f = ', relief='flat',
           command=lambda f=f_entry, x=x_entry,
                          label=s_label, func=self.calc:
                          func(None, f, x, label))

        compute.pack(side='left');  s_label.pack(side='left')
        self.master.bind('<q>', self.quit)

    def calc(self, event, f_widget, x_widget, label):
        f_txt = f_widget.get()  # get function expression as string
        # get x value as float such that we can compute with it:
        x = float(x_widget.get())
        # evaluate math expression in  f_txt as Python code:
        res = eval(f_txt)    
        label.configure(text='%g' % res)   # display f(x) value

    def quit(self, event=None): self.master.quit()

from functools import partial

class FunctionEvaluator2:
    """as FunctionEvaluator1, but avoids lambda functions"""
    
    def __init__(self, parent):
        frame = Frame(parent); frame.pack(padx=20, pady=30)
        self.master = parent
        Label(frame, text='Define f(x):').pack(side='left')

        # use get and configure to interact with the texts
        f_entry = Entry(frame, width=12)
        f_entry.pack(side='left')
        f_entry.insert('end', 'x')
        
        Label(frame, text='  x =').pack(side='left')

        x_entry = Entry(frame, width=6)
        x_entry.pack(side='left')
        x_entry.insert('end', '0')

        s_label = Label(frame, width=9) # holds the func value

        x_entry.bind('<Return>', 
           partial(self.calc, f_entry, x_entry, s_label))
           # bindings will add Event object as first argument

        compute = Button(frame, text='  f = ', relief='flat',
           command=Command(self.calc, None, f_entry, x_entry, s_label))

        compute.pack(side='left');  s_label.pack(side='left')
        self.master.bind('<q>', self.quit)

    def calc(self, f_widget, x_widget, label, event):
        f_txt = f_widget.get()  # get function expression as string
        # get x value as float such that we can compute with it:
        x = float(x_widget.get())
        # evaluate math expression in  f_txt as Python code:
        res = eval(f_txt)    
        label.configure(text='%g' % res)   # display f(x) value

    def quit(self, event=None): self.master.quit()
        
if __name__ == '__main__':
    root = Tk()                         # root (main) window
    try:
        version = sys.argv[1]
    except:
        version = '1'
    if version == '1':
        hello = FunctionEvaluator1(root)
    elif version == '2':
        hello = FunctionEvaluator2(root)
    else:
        hello = FunctionEvaluator1(root)
        
    root.mainloop()
