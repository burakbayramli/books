#!/usr/bin/env python
"""show a file in a text widget"""
from Tkinter import *
import Pmw, sys
try:    filename = sys.argv[1]
except: print 'Usage: %s filename' % sys.argv[0]; sys.exit(1)
root = Tk()            
top = Frame(root); top.pack(side='top', expand=True, fill='both')
text = Pmw.ScrolledText(top,
       borderframe=5, # a bit space around the text...
       vscrollmode='dynamic', hscrollmode='dynamic',
       labelpos='n', label_text='file %s' % filename,
       text_width=40, text_height=4,
       text_wrap='none',  # do not break too long lines
       )
text.pack(expand=True, fill='both')
# insert file as a string in the text widget:
text.insert('end', open(filename,'r').read())
Button(top, text='Quit', command=root.destroy).pack(pady=15)
root.mainloop()
