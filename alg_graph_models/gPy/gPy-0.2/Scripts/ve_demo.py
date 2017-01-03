from gPy.Examples import asia
from Tkinter import *
from gPy.Utils import scrolled_frame
for cpt in asia:
    cpt *= 1
root = Tk()
top=scrolled_frame(root,yscroll=5000, height=40000) 
windows = (Frame(top),Frame(top))
orders =(('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
          'Smoking', 'TbOrCa'),
         ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
          'Smoking', 'Dyspnea'))
asias = (asia,asia.copy())
for i in range(2):
    model, window, order = asias[i], windows[i], orders[i]
    window.pack()
    model.gui_display(window)
    for variable in order:
        model.eliminate_variable(variable)
        Label(window,text = 'Eliminating '+ variable).pack() 
        model.gui_display(window)
root.mainloop()
