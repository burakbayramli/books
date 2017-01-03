from gPy.Examples import asia
from Tkinter import Tk
from gPy.Utils import scrolled_frame
root=Tk()
top=scrolled_frame(root,yscroll=500, height=4000)
asia.gui_display(top)
asia.adg().gui_display(top)
for factor in asia:
    factor *= 1
asia.gui_display(top)
asia.adg().moralise().gui_display(top)
root.mainloop()
