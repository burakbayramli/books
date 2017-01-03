from Tkinter import Tk, Label
from gPy.Examples import asia

root=Tk()
asia.interaction_graph().gui_display(root)
Label(root,text=str(asia.hypergraph())).pack()
root.mainloop()
