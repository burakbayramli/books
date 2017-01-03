#!/usr/bin/env python
"""Show a file in a text widget. As fileshow2.py, but using the grid
   geometry manager to place widgets.
"""
from Tkinter import *
import Pmw, sys
usage = 'Usage: %s filename' % sys.argv[0]

try:    filename = sys.argv[1]
except: print usage; sys.exit(1)
root = Tk()            
top = Frame(root); top.pack(side='top', expand=True, fill='both')
text = Pmw.ScrolledText(top,
       borderframe=5, # a bit space around the text...
       vscrollmode='dynamic', hscrollmode='dynamic',
       labelpos='n', label_text='file %s' % filename,
       text_width=40, text_height=4,
       text_wrap='none',  # do not break too long lines
       )
text.grid(column=0, row=0, sticky='news')
top.rowconfigure(0, weight=1)
top.columnconfigure(0, weight=1)
# insert file as a string in the text widget:
text.insert('end', open(filename,'r').read())
Button(top, text='Quit', command=root.destroy).grid(column=0, row=1, pady=15)
root.mainloop()
