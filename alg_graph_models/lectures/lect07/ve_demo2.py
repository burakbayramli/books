from gPy.Examples import asia
from Tkinter import *
from gPy.Utils import scrolled_frame
for cpt in asia:
    cpt *= 1
asia.condition({'Bronchitis':['absent'],'XRay':['normal']})
root = Tk()
top=scrolled_frame(root,yscroll=8000, height=40000) 
windows = (Frame(top),Frame(top))
orders =(('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
          'Smoking', 'TbOrCa'),
         ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
          'Smoking', 'Dyspnea'))
asias = (asia,asia.copy())
for i in range(2):
    model, window, order = asias[i], windows[i], orders[i]
    window.pack()
    for variable in order:
        cpm = model.copy()
        step = model.eliminate_variable(variable,trace=True)
        prod_factor, message, hyperedges = step
        colours = {}
        for hyperedge in hyperedges:
            colours[hyperedge] = 'blue'
        Label(window,text = 'Eliminating '+ variable).pack() 
        cpm.gui_display(window,colours)
        fr = Frame(window)
        fr.pack()
        model[message].gui_main(fr,edit=False,bg='red').pack()
        Button(fr,text='Remove message',command=fr.destroy).pack()
root.mainloop()
