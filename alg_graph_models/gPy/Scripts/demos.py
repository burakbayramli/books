import gPy.Examples
from Tkinter import *

demo_names = []
for x in dir(gPy.Examples):
    if x[0] != '_' and x[-5:] == '_demo':
        demo_names.append(x)
demo_names.sort()

root = Tk()
for demo_name in demo_names:
    fr=Frame(root)
    fr.pack(anchor=W)
    demo = getattr(gPy.Examples,demo_name)
    Button(fr,text=demo_name,command=demo).pack(side=LEFT)
    Label(fr,text=demo.__doc__.splitlines()[0]).pack(side=LEFT)
root.mainloop()
