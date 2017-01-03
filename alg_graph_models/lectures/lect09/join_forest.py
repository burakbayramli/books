from gPy.Examples import asia
from Tkinter import *
from gPy.Utils import scrolled_frame, pretty_str_set
from gPy.Structures import JoinForest
top=scrolled_frame(Toplevel(),yscroll=500, height=400)
bottom=Toplevel()
for cpt in asia:
    cpt *= 1
asia.gui_display(top)
order1 = ('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
          'Smoking', 'TbOrCa', 'Cancer')
order2 = ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
          'Smoking', 'Dyspnea', 'Cancer')
for order in (order1, order2):
    ac = asia.copy()
    jf = JoinForest(ac.hypergraph().make_decomposable(order)[0])
    jf._uforest.gui_display(bottom,pp_vertex=pretty_str_set,width=600,height=300)
    Label(bottom,text=order).pack()
top.mainloop()
bottom.mainloop()
