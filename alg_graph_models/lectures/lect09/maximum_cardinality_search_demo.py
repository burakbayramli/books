from gPy.Examples import asia
from gPy.Structures import UGraph
from Tkinter import Tk
root = Tk()
root.title("Maximum cardinality search demonstration")
asia.adg().moralise().gui_maximum_cardinality_search(root)
UGraph(range(1,11),((1,2),(1,3),(2,3),(2,10),(3,10),(4,5),
                    (4,7),(5,6),(5,9),(5,7),(6,7),(6,9),
                    (7,8),(7,9),(8,9),(8,10),(9,10))).gui_maximum_cardinality_search(root)
root.mainloop()
