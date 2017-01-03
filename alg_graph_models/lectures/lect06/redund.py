from gPy.Examples import minibn
from Tkinter import Tk

root = Tk()
for cpt in minibn:
    cpt *= 1
minibn.gui_display(root)
minibn.red()
minibn.gui_display(root)
root.mainloop()
