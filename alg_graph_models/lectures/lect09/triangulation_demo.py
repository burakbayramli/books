from gPy.Examples import asia
from Tkinter import *
root = Tk()
orders =(['VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
          'Smoking', 'TbOrCa','Cancer'],
         ['TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
          'Smoking', 'Dyspnea','Cancer'])
ig = asia.adg().moralise()
for order in orders:
    ig.copy().gui_triangulate(root,order)
root.mainloop()
