from gPy.Examples import asia
from gPy.Models import JFM
from Tkinter import Tk
root=Tk()
jfm = JFM(asia.make_decomposable(['VisitAsia','Tuberculosis','Smoking','Cancer',
                 'TbOrCa','XRay','Bronchitis',
                 'Dyspnea']))
jfm.gui_calibrate(root)
root.mainloop()
