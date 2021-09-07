"""
Additional functions and demos for acoustics equations in
heterogeneous media.
"""
import sys, os
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from ipywidgets import interact, widgets
from IPython.display import display

def phase_plane_plot():
    "Return phase plane function ready to use with interact."

    def plot_function(pl,ul,pr,ur,rhol,bulkl,rhor,bulkr,
                      xmin=0,xmax=6,ymin=-6,ymax=6,show_phys=True,show_unphys=True):
        "Subfunction required for interactive (function of only interactive parameters)."
    
        # Define parameters
        dp = pr - pl
        du = ur - ul
        cl = np.sqrt(bulkl/rhol)
        cr = np.sqrt(bulkr/rhor)
        Zl = rhol*cl
        Zr = rhor*cr
        
        # Define eigenvectors and functions
        eig1 = np.array([-Zl, 1])
        eig2 = np.array([Zr, 1])
        lin1l = lambda p: ul - 1./Zl*(p-pl) 
        lin2l = lambda p: ul + 1./Zr*(p-pl) 
        lin1r = lambda p: ur - 1./Zl*(p-pr) 
        lin2r = lambda p: ur + 1./Zr*(p-pr) 
        
        
        # Solve Riemann problem
        al1 = (-dp + du*Zr)/(Zl + Zr)
        pm = pl - al1*Zl
        um = ul + al1
        
        # Set plot bounds
        fig, ax = plt.subplots(figsize=(8,5))
        x = (pl, pr, pm)
        y = (ul, ur, um)
        dx, dy = xmax - xmin, ymax - ymin
        ax.set_xlim(min(0.00000001,xmin),xmax)
        ax.set_ylim(ymin,ymax)
        ax.set_xlabel('Pressure (p)', fontsize=15)
        ax.set_ylabel('Velocity (u)', fontsize=15)

        p = np.linspace(xmin,xmax,500)

        if show_unphys:
            # Plot unphysical solutions
            ax.plot(p,lin2l(p),'--k')
            ax.plot(p,lin1r(p),'--k')

        if show_phys:
            # Plot physical solutions
            ax.plot(p,lin1l(p),'-k')
            ax.plot(p,lin2r(p),'-k')
            if (pm>=0 and pm <= xmax and um > ymin and um < ymax):
                ax.plot(pm, um, '-ok', markersize=10)
                ax.text(x[2] + 0.025*dx,y[2] + 0.025*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax.plot(pl, ul, '-ok', markersize=10)
        ax.plot(pr, ur, '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax.text(x[i] + 0.025*dx,y[i] + 0.025*dy,label, fontsize=15)
        plt.show()
    return plot_function


def interactive_phase_plane(ql=(10.0, -5.0),
                            qr=(40.0, 5.0),
                            paramsl=(1.0,2.0), 
                            paramsr=(2.0,8.0)):
    "Create the GUI and output the interact app."
    # Create plot function for interact
    pp_plot = phase_plane_plot()

    # Declare all widget sliders
    ql1_widget = widgets.FloatSlider(value=ql[0],min=0.01,max=50.0, description='$p_l$')
    ql2_widget = widgets.FloatSlider(value=ql[1],min=-30,max=30.0, description='$u_l$')
    qr1_widget = widgets.FloatSlider(value=qr[0],min=0.01,max=50.0, description='$p_r$')
    qr2_widget = widgets.FloatSlider(value=qr[1],min=-30,max=30.0, description='$u_r$')
    rhol_widget = widgets.FloatSlider(value=paramsl[0],min=0.01,max=10.0, description=r'$\rho_l$')
    bulkl_widget = widgets.FloatSlider(value=paramsl[1],min=0.01,max=10.0, description='$K_l$')
    rhor_widget = widgets.FloatSlider(value=paramsr[0],min=0.01,max=10.0, description=r'$\rho_r$')
    bulkr_widget = widgets.FloatSlider(value=paramsr[1],min=0.01,max=10.0, description='$K_r$')
    xmin_widget = widgets.BoundedFloatText(value=0.0000001, description='$p_{min}:$')
    xmax_widget = widgets.FloatText(value=50, description='$p_{max}:$')
    ymin_widget = widgets.FloatText(value=-30, description='$u_{min}:$')
    ymax_widget = widgets.FloatText(value=30, description='$u_{max}:$')
    rhomax_widget = widgets.FloatText(value=10, description=r'$\rho_{max}:$')
    bulkmax_widget = widgets.FloatText(value=10, description='$K_{max}:$')
    show_physical = widgets.Checkbox(value=True, description='Physical solution')
    show_unphysical = widgets.Checkbox(value=True, description='Unphysical solution')

    # Allow for dependent widgets to update
    def update_xmin(*args):
        ql3_widget.min = xmin_widget.value
        qr3_widget.min = xmin_widget.value
    def update_xmax(*args):
        ql3_widget.max = xmax_widget.value
        qr3_widget.max = xmax_widget.value
    def update_ymin(*args):
        ql2_widget.min = ymin_widget.value
        qr2_widget.min = ymin_widget.value
    def update_ymax(*args):
        ql2_widget.max = ymax_widget.value
        qr2_widget.max = ymax_widget.value
    def update_rhomax(*args):
        rhol_widget.max = rhomax_widget.value
        rhor_widget.max = rhomax_widget.value
    def update_bulkmax(*args):
        bulkl_widget.max = bulkmax_widget.value 
        bulkr_widget.max = bulkmax_widget.value 
        
    xmin_widget.observe(update_xmin, 'value')
    xmax_widget.observe(update_xmax, 'value')
    ymin_widget.observe(update_ymin, 'value')
    ymax_widget.observe(update_ymax, 'value')
    rhomax_widget.observe(update_rhomax, 'value')
    bulkmax_widget.observe(update_bulkmax, 'value')

    # Organize slider widgets into boxes
    qleftright = widgets.VBox([widgets.HBox([ql1_widget, ql2_widget]),
                               widgets.HBox([qr1_widget, qr2_widget])])
    paramswidg = widgets.VBox([widgets.HBox([rhol_widget, rhor_widget]),
                           widgets.HBox([bulkl_widget, bulkr_widget])])
    plot_opts = widgets.VBox([widgets.HBox([show_physical, show_unphysical]),
                              widgets.HBox([xmin_widget, xmax_widget, rhomax_widget]),
                              widgets.HBox([ymin_widget, ymax_widget, bulkmax_widget])])

    # Set up interactive GUI (tab style)
    interact_gui = widgets.Tab(children=[qleftright, paramswidg, plot_opts])
    interact_gui.set_title(0, 'Left and right states')
    interact_gui.set_title(1, 'Material parameters')
    interact_gui.set_title(2, 'Plot options')

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, pl=ql1_widget, ul=ql2_widget,
                        pr=qr1_widget, ur=qr2_widget,
                        rhol=rhol_widget, bulkl=bulkl_widget,
                        rhor=rhor_widget, bulkr=bulkr_widget,
                        xmin=xmin_widget, xmax=xmax_widget,
                        ymin=ymin_widget, ymax=ymax_widget,
                        show_phys=show_physical, show_unphys=show_unphysical)
    try:
        ppwidget.widget.close()
        display(interact_gui)
        display(ppwidget.widget.out)
    except:
        pass
