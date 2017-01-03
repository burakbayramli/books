#!/usr/bin/env python
from Tkinter import *
# test what kind of fonts that are used
root = Tk()
w = Label(root, text='some label')
font = w.cget('font')
import tkFont
print 'font', font, 'is actually', tkFont.Font(font=font).actual()

