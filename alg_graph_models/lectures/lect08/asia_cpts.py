from gPy.Examples import asia
from Tkinter import Tk
from gPy.Utils import scrolled_frame
root=Tk()
top=scrolled_frame(root,yscroll=5000, height=4000)
asia.gui_display(top)
asia.adg().gui_display(top,width=400,height=300)
for factor in asia:
    factor *= 1
asia.gui_display(top)
asia.adg().moralise().gui_display(top,width=400,height=300)
root.mainloop()
