import unittest
import sys

try:
    sys.argv[1]
    delay = 999999
    sys.path.insert(0,'..')
except IndexError:
    delay = 200


from gPy.Parameters import *
from gPy.Variables import *
clear_default_domain()
from gPy.Examples import *
from gPy.Graphs import *
from gPy.Hypergraphs import *
from gPy.Models import *
from gPy.IO import *
from Tkinter import *

def kill(root):
    root.destroy()


class Testdemos(unittest.TestCase):

    def test_lect04_cancer(self):
        cancer = CompactFactor(read_csv(open('cancer.dat')))
        table = cancer['Smoker', 'Cancer', 'Bronchitis']
        out = str(table)
        out += 'Number of observations is %d' % table.z()
        precision = 6
        out += str(table.normalised())
        self.assertEqual(out,open('out_lect04_cancer.txt').read())

    def test_lect05_marginalise(self):
        from gPy.Demos import _marginalise_gui
        cancer = CompactFactor(read_csv(open('cancer.dat')))
        data = cancer['Smoker', 'Cancer', 'Bronchitis']
        root=Tk()
        _marginalise_gui(data,root)
        root.after(delay,kill,root)
        root.mainloop()

    def test_lect05_marginalise2(self):
        from gPy.Demos import _marginalise_gui
        clear_default_domain()
        asia = BN()
        asia.from_dnet(read_dnet('Asia_lower.dnet'))
        factor = asia['Smoking'] * asia['Cancer'] * asia['Bronchitis']
        root=Tk()
        _marginalise_gui(factor,root)
        root.after(delay,kill,root)
        root.mainloop()


    def test_lect06_cond(self):
        from gPy.Demos import _op_gui
        clear_default_domain()
        from gPy.Demos import _div_gui
        root=Tk()
        _div_gui(minibn['Bronchitis'] * minibn['Smoking'],minibn['Smoking'],root)
        root.after(delay,kill,root)
        root.mainloop()
        root=Tk()
        _div_gui(minibn['Cancer'] * minibn['Smoking'],minibn['Smoking'],root)
        root.after(delay,kill,root)
        root.mainloop()


    def test_lect06_igs(self):
        root=Tk()
        nondecomp_norm.interaction_graph().gui_display(root)
        Label(root,text=str(nondecomp_norm.hypergraph())).pack()
        minibn.interaction_graph().gui_display(root)
        Label(root,text=str(minibn.hypergraph())).pack()
        root.after(delay,kill,root)
        root.mainloop()


    def test_lect06_norming(self):
        root = Tk()
        nondecomp.gui_display(root)
        Label(root,text='Z is %s' % nondecomp.z()).pack()
        nondecomp_norm.gui_display(root)
        root.after(delay,kill,root)
        root.mainloop()


    def test_lect06_prod_gen(self):
        from gPy.Demos import _prod_gui
        f1 = asia['Bronchitis'] * asia['Cancer']
        f2 = asia['Bronchitis'] * 1
        f3 = asia['Bronchitis'] * asia['Dyspnea']
        f4 = asia['TbOrCa'] * 1
        root = Tk()
        _prod_gui(f1,f2,root)
        root.after(delay,kill,root)
        root.mainloop()
        root = Tk()
        _prod_gui(f1,f3,root)
        root.after(delay,kill,root)
        root.mainloop()
        root = Tk()
        _prod_gui(f1,f4,root)
        root.after(delay,kill,root)
        root.mainloop()


    def test_lect06_redund(self):
        root = Tk()
        minibn.gui_display(root)
        minibn.red()
        minibn.gui_display(root)
        root.after(delay,kill,root)
        root.mainloop()

suite = unittest.makeSuite(Testdemos)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
