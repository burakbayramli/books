from Tkinter import *
from gPy.Examples import nondecomp, nondecomp_norm

root = Tk()
nondecomp.gui_display(root)
Label(root,text='Z is %s' % nondecomp.z()).pack()
nondecomp_norm.gui_display(root)
root.mainloop()
