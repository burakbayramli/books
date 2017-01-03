#!/usr/bin/env python
from Tkinter import *
from math import *

UNARY=1
BINARY=2

def add(a, b): return a + b
def neg(a): return -a
def enter(a): return a

operations = [
    ['ENTER', enter, UNARY],
    ['-', neg, UNARY],
    ['add', add, BINARY],
    ['sin', sin, UNARY],
    ['cos', cos, UNARY],
    ['tan', tan, UNARY],
    ['sinh', sinh, UNARY],
    ['cosh', cosh, UNARY],
    ['tanh', tanh, UNARY],
    ]
from functools import partial

class Calculator:

    def __init__(self, parent):

        self.master = parent   # store the parent as class member
        top = Frame(parent)    # create frame for all class widgets
        top.pack(side='top')   # pack frame in parent's window

        self.r = DoubleVar()
        self.r.set(0.0)
        self.number_window = Entry(top, width=10, textvariable=self.r)
        self.number_window.pack(pady=5)

        rowframe = Frame(top)  # frame for a row of buttons
        rowframe.pack()

        i = 0
        for button in operations:
            Button(rowframe, text=button[0], width=8,
                   command=partial(self.action, button[0],
                   button[1], button[2])).pack(side='left')
            i += 1
            if i % 4 == 0:
                print 'i=',i,'new rowframe'
                rowframe = Frame(top)
                rowframe.pack()

        self.stack = []

    def action(self, func_name, func, operator_type):
        print 'call %s as %d function' % (func_name, operator_type),
        print 'stack=',self.stack
    
        if operator_type == UNARY:
            # pull from stack, call unary function, and push the result
            # back on the stack again:
            if func_name == 'ENTER':
                self.stack.append(self.r.get())
            else:
                self.stack[-1] = func(self.r.get())
        elif operator_type == BINARY:
            # pull to values from stack, call binary function and push
            # the result back on the stack:
            r2 = self.stack[-1]
            del self.stack[-1]
            self.stack[-1] = func(self.r.get(), r2)
        else:
            print 'Wrong operator type (=%d)' % operator_type
        # ...
        self.r.set(self.stack[-1])
        print self.stack
        
        #self.master.bind('<q>', self.quit)
    


root = Tk()               # root (main) window
c = Calculator(root)
root.mainloop()
