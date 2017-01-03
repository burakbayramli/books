"""Functions providing demonstrations

@var _version: Version of this module
@type _version: String
"""

import Tkinter
import operator
from gPy.Utils import scrolled_frame

_version = '$Id: Demos.py,v 1.4 2006/11/20 09:33:57 jc Exp $'
   
def marginalise_gui(factor):
    """Generate a GUI to demonstrate marginalisation of C{factor}

    Appears in its own top level window
    @param factor: Factor to marginalise
    @type factor: L{Parameters.Factor}
    """
    root=Tkinter.Tk()
    _marginalise_gui(factor,root)
    root.mainloop()
    
def _marginalise_gui(factor,root):
    root.title('Marginalise a factor')
    top = Tkinter.Frame(root)
    top.pack()
    main = factor.gui_main(top,edit=False,varselect=True)
    varbuttons = main.grid_slaves(row=0)
    main.pack(side=Tkinter.LEFT,anchor=Tkinter.N)
    bottom = Tkinter.Frame(root)
    bottom.pack()
    for (txt,cmd) in (('Sum out selected',
                       lambda f=factor, win=top, vbs=varbuttons: _marginalise(f,win,vbs)),
                      ('Done',root.destroy)):
        button = Tkinter.Button(bottom,text=txt,command=cmd)
        button.bind('<Return>', lambda event: cmd())
        button.pack(side=Tkinter.LEFT)


def prod_gui(lhs_f,rhs_f):
    """Generate a GUI to demonstrate multiplication of factors

    Appears in its own top level window
    @param lhs_f: Factor on LHS of multiplication
    @type lhs_f: L{Parameters.Factor}
    @param rhs_f: Factor on RHS of multiplication
    @type rhs_f: L{Parameters.Factor}
    """
    root = Tkinter.Tk()
    _prod_gui(lhs_f,rhs_f,root)
    root.mainloop()

def _prod_gui(lhs_f,rhs_f,root):
    _op_gui(lhs_f,rhs_f,operator.mul,'*','multiplication',root)

def div_gui(lhs_f,rhs_f):
    """Generate a GUI to demonstrate division of factors

    Appears in its own top level window
    @param lhs_f: Factor on LHS of division
    @type lhs_f: L{Parameters.Factor}
    @param rhs_f: Factor on RHS of division
    @type rhs_f: L{Parameters.Factor}
    """
    root = Tkinter.Tk()
    _div_gui(lhs_f,rhs_f,root)
    root.mainloop()

def _div_gui(lhs_f,rhs_f,root):
    _op_gui(lhs_f,rhs_f,operator.div,'/','division',root)

def op_gui(lhs_f,rhs_f,op,op_str,title):
    """Generate a GUI to demonstrate a binary operation applied to factors

    Appears in its own top level window
    @param lhs_f: Factor on LHS of operation
    @type lhs_f: L{Parameters.Factor}
    @param rhs_f: Factor on RHS of operation
    @type rhs_f: L{Parameters.Factor}
    @param op: The binary operation to apply to the factors (e.g. muliplication)
    @type op: Binary operator
    @param op_str: Textual representation of C{op}
    @type op_str: String
    @param title: Title for demo
    @type title: String
    """
    root = Tkinter.Tk()
    _op_gui(lhs_f,rhs_f,op,op_str,title,root)
    root.mainloop()
        
def _op_gui(lhs_f,rhs_f,op,op_str,title,root):
    """Generate a GUI to demonstrate e.g. multiplication of factors

    @param lhs_f: Factor on LHS of operation
    @type lhs_f: L{Parameters.Factor}
    @param rhs_f: Factor on RHS of operation
    @type rhs_f: L{Parameters.Factor}
    @param op: The binary operation to apply to the factors (e.g. muliplication)
    @type op: Binary operator
    @param op_str: Textual representation of C{op}
    @type op_str: String
    @param title: Title for demo
    @type title: String
    @param root: Parent widget
    @type root: Suitable Tkinter object, e.g. C{Tk}, C{Tkinter.Frame}
    """
    f1, f2 = lhs_f*1, rhs_f*1
    root.title('Factor %s' % title)

    
    data = Tkinter.Frame(scrolled_frame(root))
    data.pack()
    prod_variables = f1.variables() | f2.variables()
    f1exp = f1.copy().broadcast(prod_variables)
    f2exp = f2.copy().broadcast(prod_variables)
    result = op(f1,f2)
    factors = [(f1,'',f2,'',''),
               ('=','','=','',''),
               (f1exp,op_str,f2exp,'=',result)]
    for r in range(3):
        for c in range(5):
            if r==1 or c == 1 or c==3 or (c==4 and r == 0):
                widget = Tkinter.Label(data,text=factors[r][c])
                widget.grid(row=r,column=c)
            else:
                widget = factors[r][c].gui_main(data, False, False)
                widget.grid(row=r,column=c,sticky=Tkinter.NW)
    button = Tkinter.Button(root,text='Done',command=root.destroy)
    button.bind('<Return>', lambda event: root.destroy())
    button.grid()


def _marginalise(factor,win,vbs):
    variables = []
    for cell in vbs:
        if cell['relief'] == Tkinter.SUNKEN:
            variables.append(cell.cget('text'))
    if len(variables) == len(factor.variables()):
        print "Can't sum out all variables!"
        return
    newfactor = factor.sumout(variables)
    nfm = newfactor.gui_main(win,edit=False,varselect=False)
    nfm.pack(side=Tkinter.LEFT,anchor=Tkinter.N)
