from gPy.Examples import asia
from gPy.Structures import GraphicalReducedHyperGraph
from Tkinter import Tk
hg = asia.hypergraph()
hg.red()
hg = GraphicalReducedHyperGraph(hg)
root=Tk()
gr = asia.adg().moralise()
gr.add_line('Cancer','Bronchitis')
hg2 = gr.hypergraph()
hg2.gui_grahams_algorithm(root)
root.mainloop()
