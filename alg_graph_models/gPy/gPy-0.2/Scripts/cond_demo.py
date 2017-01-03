from gPy.Examples import asia
from Tkinter import *
from gPy.Utils import scrolled_frame
for cpt in asia:
    cpt *= 1
root = Tk()
top=scrolled_frame(root,yscroll=500, height=40000)
asia.gui_display(top)
asia.condition({'Smoking':['smoker']})
asia.gui_display(top)
root.mainloop()
