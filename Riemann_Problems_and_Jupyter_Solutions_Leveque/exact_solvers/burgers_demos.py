"""
Additional functions and demos for Burgers' equation.
"""
import sys, os
from clawpack import pyclaw
from clawpack import riemann
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import animation
from IPython.display import HTML
import numpy as np
from utils import riemann_tools
from . import burgers

def multivalued_solution(t,fig=0):
    """Plots bump-into-wave figure at different times for interactive figure."""
    if fig==0:
        fig = plt.figure()
    x = np.arange(-11.0,11.0,0.1)
    y = np.exp(-x*x/10)
    x2 = 1.0*x
    x2 = x2 + t*y
    plt.plot(x, y, '--k', label = "Initial Condition")
    plt.plot(x2, y, '-k', label = r"Solution at time $t$")
    plt.xlim([-10,10])
    plt.legend(loc = 'upper left')
    plt.title('t = %.2f' % t)
    if t != 0:
        numarrows = 7
        arrowIndexList = np.linspace(len(x)/3,2*len(x)/3,numarrows, dtype = int)
        for i in arrowIndexList:
            plt.arrow(x[i], y[i], np.abs(t*y[i]-0.4), 0, head_width=0.02, head_length=0.4, fc='k', ec='k')
    if fig==0: plt.show()

def shock():
    """Returns plot function for a shock solution."""
    q_l, q_r = 5.0, 1.0
    states, speeds, reval, wave_type = burgers.exact_riemann_solution(q_l ,q_r)

    plot_function = riemann_tools.make_plot_function(states, speeds, reval, wave_type, 
                                                    layout='horizontal',
                                                    variable_names=['q'],
                                                    plot_chars=[burgers.speed])
    return plot_function

def shock_location(xshock=7.75,fig=0):
    """Plots equal-area shock figure for different shock positions for interactive figure."""
    if fig==0:
        fig = plt.figure()
    t=10
    x = np.arange(-11.0,11.0,0.05)
    y = np.exp(-x*x/10)
    x = x + t*y
    x2 = 1.0*x
    y2 = 1.0*y
    region = -1
    for i in range(len(x)):
        if (x2[i] >= xshock and region == -1):
            region = 0
            maxy = 1.0*y[i-1]
        if (x2[i] >= xshock and region == 0):
            x2[i] = 1.0*xshock
            y2[i] = 1.0*maxy
        if (x2[i] < xshock and region == 0):
            region = 1
            maxy = 1.0*y[i-1]
        if (x2[i] <= xshock and region == 1):
            x2[i] = 1.0*xshock
            y2[i] = 1.0*maxy
        if (x2[i] > xshock and region == 1):
            region = 2
    plt.plot(x, y, '-k', lw = 2, label = "Multivalued solution")
    plt.plot(x2, y2, '--r', lw = 2, label = "Shock solution")
    if (xshock == 7.75):
        plt.annotate(r"$A_1$", xy=(2, 0), xytext=(8.5,0.83), fontsize=15)
        plt.annotate(r"$A_2$", xy=(2, 0), xytext=(6.5,0.15), fontsize=15)
        plt.annotate(r"Equal Areas", xy=(2, 0), xytext=(-3,0.62), fontsize=15)
        plt.annotate(r"$A_1=A_2$", xy=(2, 0), xytext=(-2.5,0.5), fontsize=15)
    plt.xlim([-7.5,11])
    plt.legend(loc = 'upper left')
    if fig==0: plt.show()


def rarefaction_figure(t):
    """Plots rarefaction figure at different times for interactive figure."""
    numarrows = 6
    x = [-5., 0.0]
    y = [0.2, 0.2]
    for i in range(numarrows):
        x.append(0.0)
        y.append(y[0] + (i+1)*(1.0-y[0])/(numarrows+1))
    x.extend([0.0,10.0])
    y.extend([1.0,1.0])
    x2 = 1.0*np.array(x)
    x2[1:-1] = x2[1:-1] + t*np.array(y[1:-1])
    plt.plot(x, y, '--k', label = "Initial Condition")
    plt.plot(x2, y, '-k', label = r"Solution at time $t$")
    plt.xlim([-5,10])
    plt.ylim([0.0,1.2])
    plt.legend(loc = 'upper left')
    plt.title('t = %.2f' % t)
    if t != 0:
        for i in range(numarrows):
            plt.arrow(x[2+i], y[2+i], np.abs(t*y[2+i]-0.4), 0, head_width=0.02, head_length=0.4, fc='k', ec='k')
        plt.annotate(r"$q_r t$", xy=(2, 1), xytext=(t/2-0.2, 1.05), fontsize=12)
        if t > 2:
            plt.annotate(r"$q_\ell t$", xy=(2, 0), xytext=(t/8-0.4, 0.12), fontsize=12)
            plt.arrow(t/2-0.3, 1.07, -t/2+0.8, 0, head_width=0.02, head_length=0.4, fc='k', ec='k')
            plt.arrow(t/2+0.7, 1.07, t*y[-1] - t/2 - 1, 0, head_width=0.02, head_length=0.4, fc='k', ec='k')

def rarefaction():
    """Returns plot function for a rarefaction solution."""
    q_l, q_r = 2.0, 4.0
    states, speeds, reval, wave_type = burgers.exact_riemann_solution(q_l ,q_r)

    plot_function = riemann_tools.make_plot_function(states, speeds, reval, wave_type, 
                                                    layout='horizontal',
                                                    variable_names=['q'],
                                                    plot_chars=[burgers.speed])
    return plot_function

def unphysical():
    """Returns plot function for an unphysical solution."""
    q_l, q_r = 1.0, 5.0
    states, speeds, reval, wave_type = burgers.unphysical_riemann_solution(q_l ,q_r)

    plot_function = riemann_tools.make_plot_function(states, speeds, reval, wave_type, 
                                                    layout='horizontal',
                                                    variable_names=['q'],
                                                    plot_chars=[burgers.speed])
    return plot_function

def bump_animation(numframes):
    """Plots animation of solution with bump initial condition, 
    using pyclaw (calls bump_pyclaw)."""
    x, frames = bump_pyclaw(numframes) 
    fig = plt.figure()
    ax = plt.axes(xlim=(-1, 1), ylim=(-0.2, 1.2))
    line, = ax.plot([], [], '-k', lw=2)

    def fplot(frame_number):
        frame = frames[frame_number]
        pressure = frame.q[0,:]
        line.set_data(x,pressure)
        return line,

    anim = animation.FuncAnimation(fig, fplot, frames=len(frames), interval=30)
    plt.close('all')
    #return HTML(anim.to_jshtml())
    return anim.to_jshtml()

def bump_pyclaw(numframes):
    """Returns pyclaw solution of bump initial condition."""
    # Set pyclaw for burgers equation 1D
    claw = pyclaw.Controller()
    claw.tfinal = 1.5           # Set final time
    claw.keep_copy = True       # Keep solution data in memory for plotting
    claw.output_format = None   # Don't write solution data to file
    claw.num_output_times = numframes  # Number of output frames
    claw.solver = pyclaw.ClawSolver1D(riemann.burgers_1D)  # Choose burgers 1D Riemann solver
    claw.solver.all_bcs = pyclaw.BC.periodic               # Choose periodic BCs
    claw.verbosity = False                                 # Don't print pyclaw output
    domain = pyclaw.Domain( (-1.,), (1.,), (500,))         # Choose domain and mesh resolution
    claw.solution = pyclaw.Solution(claw.solver.num_eqn,domain)
    # Set initial condition
    x=domain.grid.x.centers
    claw.solution.q[0,:] = np.exp(-10 * (x)**2)     
    claw.solver.dt_initial = 1.e99
    # Run pyclaw
    status = claw.run()
    
    return x, claw.frames

def triplestate_animation(ql, qm, qr, numframes):
    """Plots animation of solution with triple-state initial condition, using pyclaw (calls  
    triplestate_pyclaw). Also plots characteristic structure by plotting contour plots of the 
    solution in the x-t plane """
    # Get solution for animation and set plot
    x, frames = triplestate_pyclaw(ql, qm, qr, numframes) 
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(9,4))
    ax1.set_xlim(-3, 3)
    ax1.set_ylim(-3, 5)
    ax2.set_xlim(-3, 3)
    ax2.set_ylim(0, 2)
    ax1.set_title('Solution q(x)')
    ax1.set_xlabel('$x$')
    ax1.set_ylabel('$q$')
    ax2.set_title('Characteristics')
    ax2.set_xlabel('$x$')
    ax2.set_ylabel('$t$')
    matplotlib.rcParams['contour.negative_linestyle'] = 'solid'
    line1, = ax1.plot([], [], '-k', lw=2)

    # Contour plot of high-res solution to show characteristic structure in xt-plane
    meshpts = 2400
    numframes2 = 600
    x2, frames2 = triplestate_pyclaw(ql, qm, qr, numframes2) 
    characs = np.zeros([numframes2,meshpts])
    xx = np.linspace(-12,12,meshpts)
    tt = np.linspace(0,2,numframes2)
    for j in range(numframes2):
        characs[j] = frames2[j].q[0]
    X,T = np.meshgrid(xx,tt)
    ax2.contour(X, T, characs, levels=np.linspace(ql, ql+0.11 ,20), linewidths=0.5, colors='k')
    ax2.contour(X, T, characs, levels=np.linspace(qm+0.11, qm+0.13 ,7), linewidths=0.5, colors='k')
    ax2.contour(X, T, characs, levels=np.linspace(qr+0.13, qr+0.2 ,15), linewidths=0.5, colors='k')
    ax2.contour(X, T, characs, 12,  linewidths=0.5, colors='k')
    #ax2.contour(X, T, characs, 38, colors='k')
    # Add animated time line to xt-plane
    line2, = ax2.plot(x, 0*x , '--k')

    line = [line1, line2]

    # Update data function for animation
    def fplot(frame_number):
        frame = frames[frame_number]
        pressure = frame.q[0,:]
        line[0].set_data(x,pressure)
        line[1].set_data(x,0*x+frame.t)
        return line

    anim = animation.FuncAnimation(fig, fplot, frames=len(frames), interval=30, blit=False)
    plt.close('all')
    #return HTML(anim.to_jshtml())
    return anim.to_jshtml()

def triplestate_pyclaw(ql, qm, qr, numframes):
    """Returns pyclaw solution of triple-state initial condition."""
    # Set pyclaw for burgers equation 1D
    meshpts = 2400 #600
    claw = pyclaw.Controller()
    claw.tfinal = 2.0           # Set final time
    claw.keep_copy = True       # Keep solution data in memory for plotting
    claw.output_format = None   # Don't write solution data to file
    claw.num_output_times = numframes  # Number of output frames
    claw.solver = pyclaw.ClawSolver1D(riemann.burgers_1D)  # Choose burgers 1D Riemann solver
    claw.solver.all_bcs = pyclaw.BC.extrap               # Choose periodic BCs
    claw.verbosity = False                                # Don't print pyclaw output
    domain = pyclaw.Domain( (-12.,), (12.,), (meshpts,))   # Choose domain and mesh resolution
    claw.solution = pyclaw.Solution(claw.solver.num_eqn,domain)
    # Set initial condition
    x=domain.grid.x.centers
    q0 = 0.0*x
    xtick1 = 900 + int(meshpts/12)
    xtick2 = xtick1 + int(meshpts/12)
    for i in range(xtick1):
	    q0[i] = ql + i*0.0001
    #q0[0:xtick1] = ql
    for i in np.arange(xtick1, xtick2):
        q0[i] = qm + i*0.0001
    #q0[xtick1:xtick2] = qm
    for i in np.arange(xtick2, meshpts):
        q0[i] = qr + i*0.0001
    #q0[xtick2:meshpts] = qr
    claw.solution.q[0,:] = q0    
    claw.solver.dt_initial = 1.e99
    # Run pyclaw
    status = claw.run()
    
    return x, claw.frames




