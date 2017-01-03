#!/usr/bin/env python
"""write latex formula in a text widget, produce corresponding GIF image"""
from Tkinter import *
import Pmw, sys, string, os

def my_os_system(cmd, verbose=1):
    """improved os.system function"""
    print '***', cmd, '***'
    failure = original_os_system(cmd)
    if failure and verbose:
        print 'execution failed'
    if failure:  return 1
    else:        return 0

original_os_system = os.system  # hold original os.system
os.system = my_os_system        # redefine os.system

class GuiMath:
    def __init__(self, parent):
        self.master = parent
        self.top = Frame(parent); self.top.pack(side='top', expand=True, fill='both')
        self.text = Pmw.ScrolledText(self.top,
              borderframe=5, # a bit space around the text...
              vscrollmode='dynamic', hscrollmode='dynamic',
              labelpos='n',
              label_text='Write a LaTeX formula in a math environment',
              text_width=40, text_height=7,
              text_wrap='none',  # do not break too long lines
                                )
        self.text.pack(expand=True, fill='both')
        br = Frame(self.top, borderwidth=5); br.pack(pady=3)
        Button(br, text='Demo text', command=self.demotext).pack(side='left')
        Button(br, text='Run LaTeX and create GIF image',
               command=self.latex).pack(side='left', padx=5)
        Button(br, text='Quit', command=self.master.destroy).pack(side='left')
        
        self.giffile = 'tmp.formula.gif'

    def latex(self):
        """run LaTeX, make PostScript, make GIF, show image"""
        f = open('tmp.tex', 'w')
        f.write(r"""
\documentclass[12pt]{article}
\pagestyle{empty}
\begin{document}
{\LARGE
%s
}
\end{document}
""" % self.text.get())
        f.close()
        failure = os.system('latex tmp.tex') 
        if failure: # latex hangs in window if failure
            text.insert('end','LaTeX error, correct...'); return
        os.system('dvips -o tmp.ps tmp.dvi')
        os.system('ps2gif tmp.ps')
        os.system("giftrans -t '#ffffff' tmp.gif > tmp2.gif")
        os.system('convert -crop 0x0 tmp2.gif %s' % self.giffile)

        # some problems with a version of convert...?
        # go to ppm and back again (destroys the giftrans, but...)
        os.system('convert %s tmp1.ppm' % self.giffile)
        os.system('convert tmp1.ppm tmp1.gif')
        os.system("giftrans -t '#ffffff' tmp1.gif > %s" % self.giffile)
        
        self.giftop = Toplevel(self.master)
        Label(self.giftop,text="GIF image in file '%s'" % self.giffile).pack()
        gif = Frame(self.giftop, borderwidth=6, relief='sunken')
        gif.pack(pady=6)
        self.image = PhotoImage(file=self.giffile)
        Label(gif, image=self.image).pack()    
        Button(self.giftop, text='Quit', command=self.giftop.destroy).pack(pady=3)
        gif.focus_set()

    def demotext(self):
        """write some math to use as a demo"""
        self.text.delete('1.0', 'end')  # delete all
        self.text.insert('end',r"""
\begin{eqnarray*}
\frac{\partial u}{\partial t} +
    \gamma\frac{\partial v}{\partial x} &=& 0,\\
\frac{\partial v}{\partial t} +
    \alpha\frac{\partial u}{\partial x} &=& 0    
\end{eqnarray*}
""")
    
root = Tk()
g = GuiMath(root)
root.mainloop()
