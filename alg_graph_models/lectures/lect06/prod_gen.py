from gPy.Examples import asia
from gPy.Demos import prod_gui

f1 = asia['Bronchitis'] * asia['Cancer']
f2 = asia['Bronchitis'] * 1
f3 = asia['Bronchitis'] * asia['Dyspnea']
f4 = asia['TbOrCa'] * 1

prod_gui(f1,f2)
prod_gui(f1,f3)
prod_gui(f1,f4)
    
