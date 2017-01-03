from gPy.IO import read_dnet
from gPy.Models import BNM
from gPy.Demos import prod_gui

asia = BNM()
asia.from_dnet(read_dnet(open('Asia_lower.dnet')))
cancer = asia['Smoking'] * asia['Cancer']
cancer = cancer.sumout(['Smoking'])
visit = asia['VisitAsia'] * 1
prod_gui(visit,cancer)




