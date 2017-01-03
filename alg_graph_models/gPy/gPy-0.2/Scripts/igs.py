from Tkinter import Tk, Label
from gPy.Examples import nondecomp_norm, minibn

root=Tk()
nondecomp_norm.interaction_graph().gui_display(root)
Label(root,text=str(nondecomp_norm.hypergraph())).pack()
minibn.interaction_graph().gui_display(root)
Label(root,text=str(minibn.hypergraph())).pack()
root.mainloop()
