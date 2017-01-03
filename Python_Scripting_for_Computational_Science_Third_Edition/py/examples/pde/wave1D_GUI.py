#!/usr/bin/env python

from scitools.TkGUI import FunctionSelector, FuncSpec, \
     UserFunction, Drawing, Parameters
from scitools.numpyutils import *
import sys, os
sys.path.insert(0, os.path.join(os.environ['scripting'], 'src',
                                'py', 'examples', 'pde'))
from wave1D_class import WaveEq2, SolverWithViz
from Tkinter import *
import Pmw, tkMessageBox

class WaveSolverWithViz(SolverWithViz):
    def do_graphics(self):
        """Plot bottom and surface."""
        # bottom: -c(x,t), surface: self.up
        if self.g is not None:
            H = self.s.physical_prm['c'](self.s.x, self.s.t)
            self.__H = -H
            #print 't=%g, max u=%g' % (self.s.t,arrmax(self.s.up))
            self.g.plotcurves([(self.__H,'H'), (self.s.up,'u')])
            self.g.configure(title='time %12.5E' % self.s.t)

    
class WaveSimGUI:
    def __init__(self, parent):
        """Build GUI, allocate solver, etc."""
        self.master = parent

        self.row_frame = Frame(self.master, borderwidth=2, relief='groove')
        self.row_frame.pack()
        Button(self.row_frame, text='Physics', command=self.set_physics,
               width=12).pack(side='left')
        Button(self.row_frame, text='Numerics', command=self.set_numerics,
               width=12).pack(side='left')
        Button(self.row_frame, text='Simulate', command=self.simulate,
               width=12).pack(side='left')
        Button(self.row_frame, text='Stop', command=self.stop,
               width=12).pack(side='left')
        Button(self.row_frame, text='Continue', command=self.continue_,
               width=12).pack(side='left')
        Button(self.row_frame, text='Quit', command=self.master.destroy,
               width=12).pack(side='left')

        self.w = WaveSolverWithViz(WaveEq2(), plot=True,
                 program='BLT', parent_frame=self.master, sleep=0)

        self._setup_shapes()

        self.nGUI = Parameters(interface='GUI')  # numerics part of GUI
        self.nGUI.add('stop time for simulation', 60.0, widget_type='entry')
        self.nGUI.add('safety factor for time step', 1.0, widget_type='entry')
        self.nGUI.add('no of grid cells', 100,
                      widget_type='slider', values=(0,1000))
        self.nGUI.add('movie speed', 1.0,
                      widget_type='slider', values=(0,1))

        self.scheme_coding = 'vectorized'

    def set_physics(self):
        """Launch dialog (Physics button in main window)."""
        self.physics_dialog = Pmw.Dialog(self.master,
             title='Set initial condition and bottom shape',
             buttons=('Apply', 'Cancel', 'Dismiss'),
             defaultbutton='Apply',
             command=self.physics_dialog_action)

        self.pGUI = {}   # physics parts of GUI
        self.pGUI['notebook'] = \
             FunctionSelector(self.physics_dialog.interior())
        self.pGUI['notebook'].add('Initial surface', self.I_list)
        self.pGUI['notebook'].add('Bottom shape', self.H_list)
        self.pGUI['notebook'].pack()
        self.pGUI['notebook'].page['Bottom shape'].\
                page['Drawing'].drawing.set_yaxis(-1.1, 0.1)
        self.pGUI['notebook'].page['Bottom shape'].\
                page['Drawing'].drawing.configure(width=500)
        self.pGUI['notebook'].page['Initial surface'].\
                page['Drawing'].drawing.configure(width=500)
        try:
            # load previously selected pages (if selected...):
            self.pGUI['notebook'].select('Initial surface',
                                         self.pGUI['I page'])
            self.pGUI['notebook'].select('Bottom shape',
                                         self.pGUI['H page'])
        except Exception, msg:
            # first time... no pages are user selected
            self.pGUI['notebook'].select('Initial surface', 'Gaussian bell')
            self.pGUI['notebook'].select('Bottom shape', 'Gaussian bell')

    def physics_dialog_action(self, result):
        # result contains the name of the button that we clicked
        if result == 'Apply':
            self.pGUI['I func'], self.pGUI['I page'] = \
            self.pGUI['notebook'].get('Initial surface')
            self.pGUI['H func'], self.pGUI['H page'] = \
                 self.pGUI['notebook'].get('Bottom shape')
        elif result == 'Dismiss':
            self.physics_dialog.destroy()  # destroy dialog window
        

    def set_numerics(self):
        self.numerics_dialog = Pmw.Dialog(self.master,
             title='Set numerical parameters',
             buttons=('Apply', 'Cancel', 'Dismiss'),
             defaultbutton='Apply',
             command=self.numerics_dialog_action)

        from scitools.TkGUI import parametersGUI
        parametersGUI(self.nGUI, self.numerics_dialog.interior())

    def numerics_dialog_action(self, result):
        if result == 'Dismiss':
            self.numerics_dialog.destroy()
        # no need to load data when Apply is pressed since
        # the data are bound to self.nGUI through Tkinter variables

    def stop(self):
        self.w.s.finished = True
        
    def simulate(self):
        try:
            # Note that H functions take the sqrt in __call__
            # so that c becomes sqrt(H)
            self.w.s.set(I=self.pGUI['I func'], c=self.pGUI['H func'])
        except (AttributeError, KeyError):
            message = 'You must set physics parameters\n'\
                      '(first press Physics button and then Apply)'
            tkMessageBox.Message(icon='info', type='ok',
                 message=message, title='About').show()
            return

        self.w.s.set(L=10, n=self.nGUI['no of grid cells'],
                     tstop=self.nGUI['stop time for simulation'],
                     safety_factor=self.nGUI['safety factor for time step'],
                     user_action=self.w.action, dt=0,
                     scheme_coding=self.scheme_coding) # 'scalar'/'vectorized'
        self.w.g.configure(sleep=1.0-self.nGUI['movie speed'])

        # ooops: user may have chosen parameters that are incompatible
        # with [-1,1] as range (can read from drawing, but not the others)
        self.w.set_graphics(ymax=1.0, ymin=-1.0, xcoor=self.w.s.x)
        #self.w.s.dump()
        self.w.s.set_ic()
        self.w.s.solve_problem()

    def continue_(self):
        # must have run simulate first, i.e., self.w.s.t must exist
        if not hasattr(self.w.s, 't'):
            message = 'You must first press Simulate, then Stop '\
                      'and then Continue'
            tkMessageBox.Message(icon='info', type='ok',
                 message=message, title='About').show()
            return

        # the user may have changed parameters, but changing n is illegal
        if self.w.s.numerical_prm['n'] != self.nGUI['no of grid cells']:
            message = 'You have changed the grid. This has no effect '\
                      'in a continued simulation. Start new simulation.'
            tkMessageBox.Message(icon='info', type='ok',
                 message=message, title='About').show()
            return
        
        self.w.s.set(tstop=self.nGUI['stop time for simulation'],
                     safety_factor=self.nGUI['safety factor for time step'])
        self.w.g.configure(sleep=1.0-self.nGUI['movie speed'])
        
        #self.w.s.dump()
        # no self.w.s.set_ic(); we start from previous state
        self.w.s.solve_problem()
        
    def _setup_shapes(self):
        # I and c functions (H=-c):

        class GaussianBell:
            """Gaussian Bell at x0 with st.dev. sigma."""
            def __init__(self, x0, sigma, amplitude):
                self.x0 = x0; self.sigma = sigma
                self.amplitude = amplitude
            def __call__(self, x):
                return self.amplitude*exp(-0.5*((x-self.x0)/self.sigma)**2)
            def __str__(self):
                return 'amplitude*exp(-0.5*((x-x0)/sigma)**2)'
            def parameters(self):
                return {'x0': self.x0, 'sigma': self.sigma,
                        'amplitude': 1.0}

        gb = GaussianBell(0, 0.5, 1.0)
        l = [FuncSpec(UserFunction, name='Gaussian bell',
                      independent_variables=['x'],
                      function_object=gb,
                      parameters=gb.parameters(),
                      formula=str(gb)),
             FuncSpec(Drawing, name='Drawing',
                      independent_variables=['x'],
                      xcoor=seq(0,10,0.02)),
             FuncSpec(UserFunction, name='Flat',
                      function_object=wrap2callable(0.0),
                      independent_variables=['x'],
                      formula='I(x)=0')]
        self.I_list = l

        class Slide1:
            """Underwater slide."""
            def __init__(self, **kwargs):
                self.d = {'Delta': -0.05, 'beta': 0.03, 'eps': 0.5, 'L': 11.0,
                          'K': 0.7, 'c': 1.2, 'alpha': -0.3, 'gamma': 0.7}
                for prm in kwargs:
                    try:
                        self.d[prm] = kwargs[prm]
                    except NameError:
                        raise NameError, \
                        'illegal constructor keyword argument "%s"' % prm
                self.__dict__.update(self.d)

            def __call__(self, x, t):
                stationary = self.Delta - self.beta*(x + self.eps)*\
                             (x + self.eps - self.L);
                slide = self.K*1.0/(sqrt(2*pi*self.gamma))*exp(\
                             -(1.0/self.gamma)*(x - \
                             (self.L + self.eps + 2)/5.0 + \
                             self.c*exp(self.alpha*t))**2)
                return sqrt(stationary - slide)  # bottom is c**2, this is c

            def parameters(self):
                return self.d

            def __str__(self):
                s = 'Delta - beta*(x+eps)*(x+eps-L) -'\
                    'K*1.0/(sqrt(2*pi*gamma))*exp('\
                    '-(1.0/gamma)*(x - (L+eps+2)/5.0 + c*exp(alpha*t))**2)'
                return s

        slide = Slide1()

        class Hdraw:
            def __init__(self):
                pass
            def attach_func(self, drawing):
                self.h = drawing
            def __call__(self, x, t=0):
                return sqrt(-self.h(x))

        class BottomBell:
            """1 - Gaussian Bell at x0 with st.dev. sigma."""
            def __init__(self, x0, sigma, amplitude):
                self.x0 = x0; self.sigma = sigma
                self.amplitude = amplitude
            def __call__(self, x, t=0):
                return sqrt(1 - self.amplitude*exp(
                    -0.5*((x-self.x0)/self.sigma)**2))
            def __str__(self):
                return '1 - amplitude*exp(-0.5*((x-x0)/sigma)**2)'
            def parameters(self):
                return {'x0': self.x0, 'sigma': self.sigma,
                        'amplitude': 0.5}

        hbell = BottomBell(3, 0.5, 0.65)
        
        # Parameters object?
        h = [FuncSpec(UserFunction, name='Unit velocity',
                      independent_variables=['x'],
                      function_object=wrap2callable(1.0),
                      #function_object=lambda x: 1, gives non-vectorized func
                      formula='H(x,t)=1'),
             FuncSpec(Drawing, name='Drawing',
                      independent_variables=['x'],
                      formula='draw depth function',
                      function_object=Hdraw(),
                      xcoor=seq(0,10,0.02)),
             FuncSpec(UserFunction, name='Slide',
                      independent_variables=['x', 't'],
                      function_object=slide,
                      parameters=slide.parameters(),
                      formula=str(slide),
                      #image='Slide1_formula1.gif',
                      scrolled_frame={'width':400,'height':300}),
             FuncSpec(UserFunction, name='Gaussian bell',
                      independent_variables=['x'],
                      function_object=hbell,
                      parameters=hbell.parameters(),
                      formula=str(hbell),),
             ]
        self.H_list = h
        
        
if __name__ == '__main__':
    root = Tk()
    Pmw.initialise(root)
    import scitools.misc; scitools.misc.fontscheme3(root)
    w = WaveSimGUI(root)
    root.mainloop()

    
