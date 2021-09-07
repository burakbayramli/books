"""
Additional functions and demos for acoustics equations.
"""
import sys, os
from clawpack import pyclaw
from clawpack import riemann
from matplotlib import animation
from IPython.display import HTML
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np
from ipywidgets import widgets
from ipywidgets import interact
from IPython.display import display
from utils import riemann_tools
from . import acoustics
colors = ['g','orange']

def decompose_q_interactive():
    """plots interactive decomposed eigenvectors."""
    pwidget = widgets.FloatSlider(min=-1,max=1,value=1.)
    uwidget = widgets.FloatSlider(min=-1,max=1,value=0.3)
    rhowidget = widgets.FloatSlider(min=0.1,max=2,value=1.,description=r'$\rho$')
    Kwidget = widgets.FloatSlider(min=0.1,max=2,value=1.)

    interact_gui = widgets.VBox([widgets.HBox([pwidget, rhowidget]),
                                   widgets.HBox([uwidget, Kwidget])]);

    mainwidget = interact(decompose_q, p=pwidget, u=uwidget, rho=rhowidget, K=Kwidget);

    try:
        mainwidget.widget.close()
        display(interact_gui)
        display(mainwidget.widget.out)
    except:
        pass

def decompose_q(p,u,K,rho):
    r"""Plotting function for decompose_q_interactive. It 
    should also print the eigenvectors and the values w_1, w_2."""
    Z = np.sqrt(K*rho)
    fig, axes = plt.subplots(1,2,figsize=(8,4)) 
    axes[0].arrow(0,0,-Z,1,head_width=0.07, head_length=0.15, 
                  color=colors[0],lw=3)
    axes[0].arrow(0,0,Z,1, head_width=0.07, head_length=0.15, 
                  color=colors[1],lw=3)
    l1 = axes[0].plot([],[],colors[0])
    l2 = axes[0].plot([],[],'-',color=colors[1])
    axes[0].set_xlim(-2,2)
    axes[0].set_ylim(-2,2)
    axes[0].set_aspect('equal')
    axes[0].set_title('Eigenvectors in phase plane',fontsize=10)
    axes[0].legend(['$r_1$','$r_2$'],loc=3)
    axes[0].plot([0,0],[-2,2],'--k',alpha=0.2)
    axes[0].plot([-2,2],[0,0],'--k',alpha=0.2)
    axes[0].set_xlabel('p')
    axes[0].set_ylabel('u')

    
    axes[1].plot([0,p],[0,u],'k-',lw=3,markersize=8)
    alpha1 = (Z*u-p)/(2.*Z)
    alpha2 = (Z*u+p)/(2.*Z)
    axes[1].plot([0,-Z*alpha1],[0,1*alpha1], color=colors[0], lw=3)
    axes[1].plot([-Z*alpha1,-Z*alpha1+Z*alpha2],[1*alpha1,alpha1+alpha2], color=colors[1], lw=3)
    axes[1].set_xlim(-1.2,1.2)
    axes[1].set_ylim(-1.2,1.2)
    axes[1].legend(['$q$',r'$w_1 r_1$',r'$w_2 r_2$'],loc='best')
    axes[1].plot([p],[u],'ko',markersize=6)
    axes[1].plot([0,0],[-2,2],'--k',alpha=0.2)
    axes[1].plot([-2,2],[0,0],'--k',alpha=0.2)
    axes[1].set_xlabel('p')
    axes[1].set_title('Decomposition of q',fontsize=10)
    plt.tight_layout()


def char_solution_interactive():
    """Plots interactive characteristics solution."""
    twidget = widgets.FloatSlider(min=0.,max=1.2,value=0.)
    rhowidget = widgets.FloatSlider(min=0.1,max=2,value=1.,description=r'$\rho$')
    Kwidget = widgets.FloatSlider(min=0.1,max=2,value=1.)

    interact_gui = widgets.HBox([widgets.VBox([twidget]), widgets.VBox([rhowidget, Kwidget])]);

    mainwidget = interact(char_solution, t=twidget, rho=rhowidget, K=Kwidget);

    try:
        mainwidget.widget.close()
        display(interact_gui)
        display(mainwidget.widget.out)
    except:
        pass

def char_solution(t, K, rho):
    """Plotting function for char_solution_interactive."""
    fig, axes = plt.subplots(1,2,figsize=(8,4))
    c = np.sqrt(K/rho)
    x = np.linspace(-2*c-1,2*c+1,41)
    tt = np.linspace(0,1.2,20)
    for ix in x:
        axes[0].plot(ix-c*tt,tt,'-k',lw=0.5,color=colors[0])
        axes[0].plot(ix+c*tt,tt,'-k',lw=0.5,color=colors[1])
    axes[0].set_xlim(-1,1)
    axes[0].set_ylim(-0.2,1.2)
    axes[0].set_title('Characteristics')
    axes[0].set_xlabel('$x$')
    axes[0].set_ylabel('$t$')
    
    xx = np.linspace(-2*c-1,2*c+1,1000)
    w120 = lambda x: -0.1*np.exp(-50*x**2)
    w220 = lambda x:  0.1*np.exp(-50*x**2)
    spacing = 1
    l1, = axes[0].plot(xx,w120(xx+c*spacing*t)+spacing*t,color=colors[0],lw=2,label='$w_{1}$')
    l2, = axes[0].plot(xx,w220(xx-c*spacing*t)+spacing*t,color=colors[1],lw=2,label='$w_{2}$')
    axes[0].legend(handles=[l1,l2], loc=4)
    axes[1].plot(xx,w120(xx-c*spacing*t)+w220(xx+c*spacing*t)+spacing*t,'-k',lw=2)
    axes[1].set_xlim(-1,1)
    axes[1].set_ylim(-0.2,1.2)
    axes[1].set_title('Velocity')
    axes[1].set_xlabel('$x$')
    
    plt.tight_layout()


def phase_plane_plot():
    """Plots phase plane, also used by interactive_phase_plane
    since it returns phase plane function ready to use with interact."""

    def plot_function(pl,ul,pr,ur,rho,bulk,
                      xmin=0,xmax=6,ymin=-6,ymax=6):
        "Subfunction required for interactive (function of only interactive parameters)."
    
        # Define parameters
        dp = pr - pl
        du = ur - ul
        c = np.sqrt(bulk/rho)
        Z = rho*c
        
        # Define eigenvectors and functions
        eig1 = np.array([-Z, 1])
        eig2 = np.array([Z, 1])
        lin1l = lambda p: ul - 1./Z*(p-pl) 
        lin2l = lambda p: ul + 1./Z*(p-pl) 
        lin1r = lambda p: ur - 1./Z*(p-pr) 
        lin2r = lambda p: ur + 1./Z*(p-pr) 
        
        
        # Solve Riemann problem
        al1 = (-dp + du*Z)/(2*Z)
        pm = pl - al1*Z
        um = ul + al1
        
        # Set plot bounds
        fig, ax = plt.subplots(figsize=(5,4))
        x = (pl, pr, pm)
        y = (ul, ur, um)
        dx, dy = xmax - xmin, ymax - ymin
        ax.set_xlim(min(0.00000001,xmin),xmax)
        ax.set_ylim(ymin,ymax)
        ax.set_xlabel('Pressure (p)', fontsize=15)
        ax.set_ylabel('Velocity (u)', fontsize=15)

        p = np.linspace(xmin,xmax,500)


        # Plot incorrect solutions
        ax.plot(p,lin2l(p),'--k')
        ax.plot(p,lin1r(p),'--k')


        # Plot physical solution
        ax.plot(p,lin1l(p),'-k')
        ax.plot(p,lin2r(p),'-k')
        if (pm>=0 and pm <= xmax and um > ymin and um < ymax):
            ax.plot(pm, um, '-ok', markersize=10)
            ax.text(x[2] + 0.03*dx,y[2] + 0.03*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax.plot(pl, ul, '-ok', markersize=10)
        ax.plot(pr, ur, '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax.text(x[i] + 0.03*dx,y[i] + 0.03*dy,label, fontsize=15)
        plt.show()
    return plot_function


def interactive_phase_plane(ql=(10.0, -5.0), qr=(40.0, 5.0), rho=2.0, bulk=1.0):
    """Plots interactive phase plane plot."""

    # Create plot function for interact
    pp_plot = phase_plane_plot()

    # Declare all widget sliders
    ql1_widget = widgets.FloatSlider(value=ql[0],min=0.01,max=10.0, description='$p_l$')
    ql2_widget = widgets.FloatSlider(value=ql[1],min=-10,max=10.0, description='$u_l$')
    qr1_widget = widgets.FloatSlider(value=qr[0],min=0.01,max=10.0, description='$p_r$')
    qr2_widget = widgets.FloatSlider(value=qr[1],min=-10,max=10.0, description='$u_r$')
    rho_widget = widgets.FloatSlider(value=rho,min=0.01,max=10.0, description=r'$\rho$')
    bulk_widget = widgets.FloatSlider(value=bulk,min=0.01,max=10.0, description='$K$')
    xmin_widget = widgets.BoundedFloatText(value=0.0000001, description='$p_{min}:$')
    xmax_widget = widgets.FloatText(value=10, description='$p_{max}:$')
    ymin_widget = widgets.FloatText(value=-5, description='$u_{min}:$')
    ymax_widget = widgets.FloatText(value=5, description='$u_{max}:$')

    # Allow for dependent widgets to update
    def update_xmin(*args):
        ql1_widget.min = xmin_widget.value
        qr1_widget.min = xmin_widget.value
    def update_xmax(*args):
        ql1_widget.max = xmax_widget.value
        qr1_widget.max = xmax_widget.value
    def update_ymin(*args):
        ql2_widget.min = ymin_widget.value
        qr2_widget.min = ymin_widget.value
    def update_ymax(*args):
        ql2_widget.max = ymax_widget.value
        qr2_widget.max = ymax_widget.value
    xmin_widget.observe(update_xmin, 'value')
    xmax_widget.observe(update_xmax, 'value')
    ymin_widget.observe(update_ymin, 'value')
    ymax_widget.observe(update_ymax, 'value')

    # Organize slider widgets into boxes
    qleftright = widgets.VBox([widgets.HBox([ql1_widget, ql2_widget, rho_widget]),
                               widgets.HBox([qr1_widget, qr2_widget, bulk_widget])])
    plot_opts = widgets.VBox([widgets.HBox([xmin_widget, xmax_widget]),
                              widgets.HBox([ymin_widget, ymax_widget])])

    # Set up interactive GUI (tab style)
    interact_gui = widgets.Tab(children=[qleftright, plot_opts])
    interact_gui.set_title(0, 'Left and right states')
    interact_gui.set_title(1, 'Plot options')

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, pl=ql1_widget, ul=ql2_widget,
                        pr=qr1_widget, ur=qr2_widget,
                        rho=rho_widget, bulk=bulk_widget,
                        xmin=xmin_widget, xmax=xmax_widget,
                        ymin=ymin_widget, ymax=ymax_widget)
    try:
        ppwidget.widget.close()
        display(interact_gui)
        display(ppwidget.widget.out)
    except:
        pass


def full_riemann_solution_plot():
    """Plots full Riemann solution, including the phase plane, also used by 
    full_riemann_interactive  and riemann_plot_func_pplane since it returns plot 
    function ready to use with interact."""
    
    def plot_function(t,pl,ul,pr,ur,rho,bulk,which_char, xmin=0,xmax=6,ymin=-6,ymax=6):
        "Subfunction required for interactive (function of only interactive parameters)."
    
        # Define parameters
        dp = pr - pl
        du = ur - ul
        c = np.sqrt(bulk/rho)
        Z = rho*c
        
        # Define eigenvectors and functions
        eig1 = np.array([-Z, 1])
        eig2 = np.array([Z, 1])
        lin1l = lambda p: ul - 1./Z*(p-pl) 
        lin2l = lambda p: ul + 1./Z*(p-pl) 
        lin1r = lambda p: ur - 1./Z*(p-pr) 
        lin2r = lambda p: ur + 1./Z*(p-pr) 
        
        
        # Solve Riemann problem       
        aux = [rho,bulk]
        states, speeds, riemann_eval = acoustics.exact_riemann_solution(np.array([pl,ul]), np.array([pr,ur]), aux)
        pm = states[0][1]
        um = states[1][1]

        
        # Set figure grid
        fig = plt.figure(figsize=(10,5)) #figsize=(11.5, 5.5))
        outer_grid = gridspec.GridSpec(1, 2, wspace=0.15, hspace=0.15)
        inner_grid = gridspec.GridSpecFromSubplotSpec(3, 1, subplot_spec=outer_grid[0], wspace=0.0, hspace=0.0)
        ax1 = plt.Subplot(fig, inner_grid[0]) # x-t plane
        ax2 = plt.Subplot(fig, inner_grid[1]) # x vs pressure
        ax3 = plt.Subplot(fig, inner_grid[2]) # x vs velocity
        ax4 = plt.Subplot(fig, outer_grid[1]) # phase plane
        ax1.set_ylabel("t", fontsize=10)
        ax2.set_ylabel("pressure", fontsize=10)
        ax3.set_ylabel("velocity", fontsize=10)
        ax3.set_xlabel("x", fontsize=10)
        ax1.set_xticks([])
        ax2.set_xticks([])

        # Plot Riemann solution on ax1, ax2 and ax3
        ax = np.array([ax1, ax2, ax3])
        riemann_tools.plot_riemann(states, speeds, riemann_eval, wave_types=None, t=t, ax=ax, 
                                   layout='vertical', variable_names=['pressure', 'velocity'])
        
        # Plot characteristics on ax1 if required
        if which_char:
            plot_chars=[acoustics.lambda1, acoustics.lambda2]
            riemann_tools.plot_characteristics(riemann_eval, plot_chars[which_char-1],
                                     aux=(np.array(aux),np.array(aux)), axes=ax[0], speeds=speeds)
        
        # Plot solution in phase plane plot ion ax4
        x = (pl, pr, pm)
        y = (ul, ur, um)
        dx, dy = xmax - xmin, ymax - ymin
        ax4.set_xlim(min(0.00000001,xmin),xmax)
        ax4.set_ylim(ymin,ymax)
        ax4.set_xlabel('Pressure (p)', fontsize=10)
        ax4.set_ylabel('Velocity (u)', fontsize=10)
        ax4.set_title('Phase plane', fontsize=12)
        p = np.linspace(xmin,xmax,500)

        # Plot incorrect solution
        ax4.plot(p,lin2l(p),'--k')
        ax4.plot(p,lin1r(p),'--k')

        # Plot physical solution
        ax4.plot(p,lin1l(p),'-k')
        ax4.plot(p,lin2r(p),'-k')
        if (pm>=0 and pm <= xmax and um > ymin and um < ymax):
            ax4.plot(pm, um, '-ok', markersize=10)
            ax4.text(x[2] + 0.03*dx,y[2] + 0.03*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax4.plot(pl, ul, '-ok', markersize=10)
        ax4.plot(pr, ur, '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax4.text(x[i] + 0.03*dx,y[i] + 0.03*dy,label, fontsize=15)
            
        # Add all plots to fig and show    
        fig.add_subplot(ax1)
        fig.add_subplot(ax2)
        fig.add_subplot(ax3)
        fig.add_subplot(ax4)
        plt.show()
    return plot_function

def full_riemann_solution_plot_fixed(ql,qr,rho,bulk):
    """Plots full Riemann solution (with some fixed parameters), including the phase 
    plane, also used by full_riemann_interactive  and riemann_plot_func_pplane since it 
    returns plot function ready to use with interact."""
    
    def plot_function(t,which_char, xmin=0,xmax=6,ymin=-6,ymax=6):
        "Subfunction required for interactive (function of only interactive parameters)."
    
        # Define parameters
        pl = ql[0]
        ul = ql[1]
        pr = qr[0] 
        ur = qr[1]
        dp = pr - pl
        du = ur - ul
        c = np.sqrt(bulk/rho)
        Z = rho*c
        
        # Define eigenvectors and functions
        eig1 = np.array([-Z, 1])
        eig2 = np.array([Z, 1])
        lin1l = lambda p: ul - 1./Z*(p-pl) 
        lin2l = lambda p: ul + 1./Z*(p-pl) 
        lin1r = lambda p: ur - 1./Z*(p-pr) 
        lin2r = lambda p: ur + 1./Z*(p-pr) 
        
        
        # Solve Riemann problem       
        aux = [rho,bulk]
        states, speeds, riemann_eval = acoustics.exact_riemann_solution(np.array([pl,ul]), np.array([pr,ur]), aux)
        pm = states[0][1]
        um = states[1][1]

        
        # Set figure grid
        fig = plt.figure(figsize=(10,5)) #figsize=(11.5, 5.5))
        outer_grid = gridspec.GridSpec(1, 2, wspace=0.15, hspace=0.15)
        inner_grid = gridspec.GridSpecFromSubplotSpec(3, 1, subplot_spec=outer_grid[0], wspace=0.0, hspace=0.0)
        ax1 = plt.Subplot(fig, inner_grid[0]) # x-t plane
        ax2 = plt.Subplot(fig, inner_grid[1]) # x vs pressure
        ax3 = plt.Subplot(fig, inner_grid[2]) # x vs velocity
        ax4 = plt.Subplot(fig, outer_grid[1]) # phase plane
        ax1.set_ylabel("t", fontsize=10)
        ax2.set_ylabel("pressure", fontsize=10)
        ax3.set_ylabel("velocity", fontsize=10)
        ax3.set_xlabel("x", fontsize=10)
        ax1.set_xticks([])
        ax2.set_xticks([])

        # Plot Riemann solution on ax1, ax2 and ax3
        ax = np.array([ax1, ax2, ax3])
        riemann_tools.plot_riemann(states, speeds, riemann_eval, wave_types=None, t=t, ax=ax, 
                                   layout='vertical', variable_names=['pressure', 'velocity'])
        
        # Plot characteristics on ax1 if required
        if which_char:
            plot_chars=[acoustics.lambda1, acoustics.lambda2]
            riemann_tools.plot_characteristics(riemann_eval, plot_chars[which_char-1],
                                     aux=(np.array(aux),np.array(aux)), axes=ax[0], speeds=speeds)
        
        # Plot solution in phase plane plot ion ax4
        x = (pl, pr, pm)
        y = (ul, ur, um)
        dx, dy = xmax - xmin, ymax - ymin
        ax4.set_xlim(min(0.00000001,xmin),xmax)
        ax4.set_ylim(ymin,ymax)
        ax4.set_xlabel('Pressure (p)', fontsize=10)
        ax4.set_ylabel('Velocity (u)', fontsize=10)
        ax4.set_title('Phase plane', fontsize=12)
        p = np.linspace(xmin,xmax,500)

        # Plot incorrect solution
        ax4.plot(p,lin2l(p),'--k')
        ax4.plot(p,lin1r(p),'--k')

        # Plot physical solution
        ax4.plot(p,lin1l(p),'-k')
        ax4.plot(p,lin2r(p),'-k')
        if (pm>=0 and pm <= xmax and um > ymin and um < ymax):
            ax4.plot(pm, um, '-ok', markersize=10)
            ax4.text(x[2] + 0.03*dx,y[2] + 0.03*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax4.plot(pl, ul, '-ok', markersize=10)
        ax4.plot(pr, ur, '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax4.text(x[i] + 0.03*dx,y[i] + 0.03*dy,label, fontsize=15)
            
        # Add all plots to fig and show    
        fig.add_subplot(ax1)
        fig.add_subplot(ax2)
        fig.add_subplot(ax3)
        fig.add_subplot(ax4)
        plt.show()
    return plot_function

def riemann_plot_pplane(ql=(10.0, -5.0), qr=(40.0, 5.0), rho=2.0, bulk=1.0):
    """Plots interactive riemann solution with time dependence and phase plane plot."""

    # Create plot function for interact
    pp_plot = full_riemann_solution_plot_fixed(ql,qr,rho,bulk)

    # Declare all widget sliders
    t_widget = widgets.FloatSlider(value=0,min=0.0,max=1.0, description='$t$')
    which_char_widget = widgets.Dropdown(options=[None,1,2],description='Characs.')

    # Set up interactive GUI
    interact_gui = widgets.HBox([t_widget, which_char_widget])

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, t=t_widget, 
                        which_char=which_char_widget)
        
    try:
        ppwidget.widget.close()
        display(interact_gui)
        display(ppwidget.widget.out)
    except:
        pass


def full_riemann_interactive(ql=(10.0, -5.0), qr=(40.0, 5.0), rho=2.0, bulk=1.0):
    """Plots interactive full riemann solution with phase plane plot."""

    # Create plot function for interact
    pp_plot = full_riemann_solution_plot()

    # Declare all widget sliders
    t_widget = widgets.FloatSlider(value=0,min=0.0,max=1.0, description='$t$')
    ql1_widget = widgets.FloatSlider(value=ql[0],min=0.01,max=50.0, description='$p_l$')
    ql2_widget = widgets.FloatSlider(value=ql[1],min=-30,max=30.0, description='$u_l$')
    qr1_widget = widgets.FloatSlider(value=qr[0],min=0.01,max=50.0, description='$p_r$')
    qr2_widget = widgets.FloatSlider(value=qr[1],min=-30,max=30.0, description='$u_r$')
    rho_widget = widgets.FloatSlider(value=rho,min=0.01,max=10.0, description=r'$\rho$')
    bulk_widget = widgets.FloatSlider(value=bulk,min=0.01,max=10.0, description='$K$')
    xmin_widget = widgets.BoundedFloatText(value=0.0000001, description='$p_{min}:$')
    xmax_widget = widgets.FloatText(value=50, description='$p_{max}:$')
    ymin_widget = widgets.FloatText(value=-30, description='$u_{min}:$')
    ymax_widget = widgets.FloatText(value=30, description='$u_{max}:$')
    which_char_widget = widgets.Dropdown(options=[None,1,2],description='Characs.')

    # Allow for dependent widgets to update
    def update_xmin(*args):
        ql1_widget.min = xmin_widget.value
        qr1_widget.min = xmin_widget.value
    def update_xmax(*args):
        ql1_widget.max = xmax_widget.value
        qr1_widget.max = xmax_widget.value
    def update_ymin(*args):
        ql2_widget.min = ymin_widget.value
        qr2_widget.min = ymin_widget.value
    def update_ymax(*args):
        ql2_widget.max = ymax_widget.value
        qr2_widget.max = ymax_widget.value
    xmin_widget.observe(update_xmin, 'value')
    xmax_widget.observe(update_xmax, 'value')
    ymin_widget.observe(update_ymin, 'value')
    ymax_widget.observe(update_ymax, 'value')

    # Organize slider widgets into boxes
    qleftright = widgets.VBox([widgets.HBox([t_widget, which_char_widget]), 
                               widgets.HBox([ql1_widget, ql2_widget, rho_widget]),
                               widgets.HBox([qr1_widget, qr2_widget, bulk_widget])])
    plot_opts = widgets.VBox([widgets.HBox([xmin_widget, xmax_widget]),
                              widgets.HBox([ymin_widget, ymax_widget])])

    # Set up interactive GUI (tab style)
    interact_gui = widgets.Tab(children=[qleftright, plot_opts])
    interact_gui.set_title(0, 'Left and right states')
    interact_gui.set_title(1, 'Plot options')

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, t=t_widget, 
                        pl=ql1_widget, ul=ql2_widget,
                        pr=qr1_widget, ur=qr2_widget,
                        rho=rho_widget, bulk=bulk_widget,
                        which_char=which_char_widget,
                        xmin=xmin_widget, xmax=xmax_widget,
                        ymin=ymin_widget, ymax=ymax_widget)
    try:
        ppwidget.widget.close()
        display(interact_gui)
        display(ppwidget.widget.out)
    except:
        pass

def bump_animation(numframes):
    """Plots animation of solution with bump initial condition, 
    using pyclaw (calls bump_pyclaw)."""
    x, frames = bump_pyclaw(numframes) 
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(9,4))
    ax1.set_xlim(-2, 2)
    ax1.set_ylim(-3, 5)
    ax1.set_xlabel('$x$')
    ax1.set_ylabel('Pressure: $p$')
    ax2.set_xlim(-2, 2)
    ax2.set_ylim(-3, 3)
    ax2.set_xlabel('$x$')
    ax2.set_ylabel('Velocity: $u$')
    line1, = ax1.plot([], [], '-k', lw=2)
    line2, = ax2.plot([], [], '-k', lw=2)
    line = [line1, line2]

    def fplot(frame_number):
        frame = frames[frame_number]
        pressure = frame.q[0,:]
        velocity = frame.q[1,:]
        line[0].set_data(x,pressure)
        line[1].set_data(x,velocity)
        return line,

    anim = animation.FuncAnimation(fig, fplot, frames=len(frames), interval=30)
    plt.close('all')
    return anim.to_jshtml()


def bump_pyclaw(numframes):
    """Returns pyclaw solution of bump initial condition."""
    # Set pyclaw for burgers equation 1D
    claw = pyclaw.Controller()
    claw.tfinal = 5.0          # Set final time
    claw.keep_copy = True       # Keep solution data in memory for plotting
    claw.output_format = None   # Don't write solution data to file
    claw.num_output_times = numframes  # Number of output frames
    claw.solver = pyclaw.ClawSolver1D(riemann.acoustics_1D)  # Choose acoustics 1D Riemann solver
    claw.solver.all_bcs = pyclaw.BC.wall              # Choose periodic BCs
    claw.verbosity = False                                 # Don't print pyclaw output
    domain = pyclaw.Domain( (-2.,), (2.,), (800,))         # Choose domain and mesh resolution
    claw.solution = pyclaw.Solution(claw.solver.num_eqn, domain)
    # Set initial condition
    x=domain.grid.x.centers
    claw.solution.q[0,:] = 4.0*np.exp(-10 * (x-1.0)**2)  
    claw.solution.q[1,:] = 0.0        
    claw.solver.dt_initial = 1.e99
    # Set parameters
    rho = 1.0
    bulk = 1.0
    claw.solution.problem_data['rho'] = rho
    claw.solution.problem_data['bulk'] = bulk
    claw.solution.problem_data['zz'] = np.sqrt(rho*bulk)
    claw.solution.problem_data['cc'] = np.sqrt(bulk/rho)
    # Run pyclaw
    status = claw.run()
    
    return x, claw.frames

