"""
Interactive phase plane plot for Euler equations with ideal gas,
Euler equations with Tammann equations of state and acoustic equations.
"""
import sys, os
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from ipywidgets import widgets
from ipywidgets import interact
from IPython.display import display

def euler_phase_plane_plot():
    "Return phase plane function ready to use with interact."

    # Define hugoniot locus and intergal curves independently (needed for interact version)
    def hugoniot_locus_1(p,ql,gamma):
        rhol, ul, pl = ql
        cl = np.sqrt(gamma*pl/rhol)
        beta = (gamma+1.)/(gamma-1.)
        return ul + 2*cl/np.sqrt(2*gamma*(gamma-1.)) * ((1-p/pl)/np.sqrt(1+beta*p/pl))

    def hugoniot_locus_3(p,qr,gamma):
        rhor, ur, pr = qr
        cr = np.sqrt(gamma*pr/rhor)
        beta = (gamma+1.)/(gamma-1.)
        return ur - 2*cr/np.sqrt(2*gamma*(gamma-1.)) * ((1-p/pr)/np.sqrt(1+beta*p/pr))

    def integral_curve_1(p,ql,gamma):
        rhol, ul, pl = ql
        cl = np.sqrt(gamma*pl/rhol)
        return ul + 2*cl/(gamma-1.)*(1.-(p/pl)**((gamma-1.)/(2.*gamma)))

    def integral_curve_3(p,qr,gamma):
        rhor, ur, pr = qr
        cr = np.sqrt(gamma*pr/rhor)
        return ur - 2*cr/(gamma-1.)*(1.-(p/pr)**((gamma-1.)/(2.*gamma)))

    def plot_function(rhol,ul,pl,rhor,ur,pr,gamma,
                      xmin,xmax,ymin,ymax,show_phys,show_unphys):
        "Subfunction required for interactive (function of only interactive parameters)."
        ql = [rhol, ul, pl]
        qr = [rhor, ur, pr]

        hugoloc1 = lambda p: hugoniot_locus_1(p,ql,gamma)
        hugoloc3 = lambda p: hugoniot_locus_3(p,qr,gamma)
        intcurv1 = lambda p: integral_curve_1(p,ql,gamma)
        intcurv3 = lambda p: integral_curve_3(p,qr,gamma)

        def phi_l(p):
            "Check whether the 1-wave is a shock or rarefaction."
            if p >= pl:
                return hugoloc1(p)
            else:
                return intcurv1(p)

        # Check whether the 3-wave is a shock or rarefaction
        def phi_r(p):
            if p >= pr:
                return hugoloc3(p)
            else:
                return intcurv3(p)

        phi = lambda p: phi_l(p)-phi_r(p)

        # Use fsolve to find p_star such that Phi(p_star)=0
        p0 = (ql[2] + qr[2])/2.0  # initial guess is the average of initial pressures
        p_star, info, ier, msg = fsolve(phi, p0, full_output=True, xtol=1.e-14)
        # For strong rarefactions, sometimes fsolve needs help
        if ier != 1:
            p_star, info, ier, msg = fsolve(phi, p0, full_output=True, factor=0.1, xtol=1.e-10)
            # This should not happen:
            if ier != 1:
                print('Warning: fsolve did not converge.')
        u_star = 0.5*(phi_l(p_star) + phi_r(p_star))

        # Set plot bounds
        fig, ax = plt.subplots(figsize=(12,4))
        x = (ql[2], qr[2], p_star)
        y = (ql[1], qr[1], u_star)
        dx, dy = xmax - xmin, ymax - ymin
        ax.set_xlim(min(0.00000001,xmin),xmax)
        ax.set_ylim(ymin,ymax)
        ax.set_xlabel('Pressure (p)', fontsize=15)
        ax.set_ylabel('Velocity (u)', fontsize=15)

        p = np.linspace(xmin,xmax,500)
        p1_shk = p[p>=pl]
        p1_rar = p[p<pl]
        p3_shk = p[p>=pr]
        p3_rar = p[p<pr]

        if show_unphys:
            # Plot unphysical solutions
            ax.plot(p1_rar,hugoloc1(p1_rar),'--r')
            ax.plot(p3_rar,hugoloc3(p3_rar),'--r')
            ax.plot(p1_shk,intcurv1(p1_shk),'--b')
            ax.plot(p3_shk,intcurv3(p3_shk),'--b')

        if show_phys:
            # Plot physical solutions
            ax.plot(p1_shk,hugoloc1(p1_shk),'-r')
            ax.plot(p3_shk,hugoloc3(p3_shk),'-r')
            ax.plot(p1_rar,intcurv1(p1_rar),'-b')
            ax.plot(p3_rar,intcurv3(p3_rar),'-b')
            if (p_star <= xmax and u_star >ymin and u_star < ymax):
                ax.plot(p_star, u_star, '-ok', markersize=10)
                ax.text(x[2] + 0.025*dx,y[2] + 0.025*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax.plot(ql[2], ql[1], '-ok', markersize=10)
        ax.plot(qr[2], qr[1], '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax.text(x[i] + 0.025*dx,y[i] + 0.025*dy,label, fontsize=15)
        plt.show()
    return plot_function


def euler_interactive_phase_plane(ql=(1.0, -3.0, 100.0),
                                  qr=(1.0, 3.0, 100.0),
                                  gamma=1.4):
    "Create the GUI and output the interact app."
    # Create plot function for interact
    pp_plot = euler_phase_plane_plot()

    # Declare all widget sliders
    ql1_widget = widgets.FloatSlider(value=ql[0],min=0.01,max=100.0, description=r'$\rho_l$')
    ql2_widget = widgets.FloatSlider(value=ql[1],min=-15,max=15.0, description='$u_l$')
    ql3_widget = widgets.FloatSlider(value=ql[2],min=1,max=200.0, description='$p_l$')
    qr1_widget = widgets.FloatSlider(value=qr[0],min=0.01,max=100.0, description=r'$\rho_r$')
    qr2_widget = widgets.FloatSlider(value=qr[1],min=-15,max=15.0, description='$u_r$')
    qr3_widget = widgets.FloatSlider(value=qr[2],min=1,max=200.0, description='$p_r$')
    gamm_widget = widgets.FloatSlider(value=gamma,min=0.01,max=10.0, description='$\gamma$')
    xmin_widget = widgets.BoundedFloatText(value=0.0000001, description='$p_{min}:$')
    xmax_widget = widgets.FloatText(value=200, description='$p_{max}:$')
    ymin_widget = widgets.FloatText(value=-15, description='$u_{min}:$')
    ymax_widget = widgets.FloatText(value=15, description='$u_{max}:$')
    show_physical = widgets.Checkbox(value=True, description='Physical solution')
    show_unphysical = widgets.Checkbox(value=True, description='Unphysical solution')
    # Additional control widgets not called by function
    rhomax_widget = widgets.FloatText(value=100, description=r'$\rho_{max}$')
    gammax_widget = widgets.FloatText(value=10, description='$\gamma_{max}$')

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
        ql1_widget.max = rhomax_widget.value
        qr1_widget.max = rhomax_widget.value
    def update_gammax(*args):
        gamm_widget.max = gammax_widget.value
    xmin_widget.observe(update_xmin, 'value')
    xmax_widget.observe(update_xmax, 'value')
    ymin_widget.observe(update_ymin, 'value')
    ymax_widget.observe(update_ymax, 'value')
    rhomax_widget.observe(update_rhomax, 'value')
    gammax_widget.observe(update_gammax, 'value')

    # Organize slider widgets into boxes
    qleftright = widgets.VBox([widgets.HBox([ql1_widget, ql2_widget, ql3_widget]),
                               widgets.HBox([qr1_widget, qr2_widget, qr3_widget]),
                               widgets.HBox([gamm_widget])])
    plot_opts = widgets.VBox([widgets.HBox([show_physical, show_unphysical]),
                              widgets.HBox([xmin_widget, xmax_widget, rhomax_widget]),
                              widgets.HBox([ymin_widget, ymax_widget, gammax_widget])])

    # Set up interactive GUI (tab style)
    interact_gui = widgets.Tab(children=[qleftright, plot_opts])
    interact_gui.set_title(0, 'Left and right states')
    interact_gui.set_title(1, 'Plot options')

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, rhol=ql1_widget, ul=ql2_widget, pl=ql3_widget,
                        rhor=qr1_widget, ur=qr2_widget, pr=qr3_widget,
                        gamma=gamm_widget,
                        xmin=xmin_widget, xmax=xmax_widget,
                        ymin=ymin_widget, ymax=ymax_widget,
                        show_phys=show_physical, show_unphys=show_unphysical)
    try:
        ppwidget.widget.close()
        display(interact_gui)
        display(ppwidget.widget.out)
    except:
        pass


def euler_tammann_phase_plane_plot():
    "Return phase plane function ready to use with interact."
    # Define hugoniot locus and integral curves independently (needed for interact version)
    def hugoniot_locus_1(p,ql,params):
        gammal, pinfl = params
        rhol, ul, pl = ql
        betal = (pl + pinfl)*(gammal - 1.0)/(gammal + 1.0)
        alphal = 2.0/((gammal + 1.0)*rhol)
        return ul - (p - pl)*np.sqrt(alphal/(p + pinfl + betal))

    def hugoniot_locus_3(p,qr,params):
        gammar, pinfr =  params
        rhor, ur, pr = qr
        betar = (pr + pinfr)*(gammar - 1.0)/(gammar + 1.0)
        alphar = 2.0/((gammar + 1.0)*rhor)
        return ur + (p - pr)*np.sqrt(alphar/(p + pinfr + betar))

    def integral_curve_1(p,ql,params):
        gammal, pinfl =  params
        rhol, ul, pl = ql
        cl =  np.sqrt(gammal*(pl + pinfl)/rhol)
        gl1 = gammal - 1.0
        return ul + 2*cl/gl1*(1 - ((p + pinfl)/(pl+pinfl))**(gl1/(2.0*gammal)))

    def integral_curve_3(p,qr,params):
        gammar, pinfr =  params
        rhor, ur, pr = qr
        cr =  np.sqrt(gammar*(pr + pinfr)/rhor)
        gr1 = gammar - 1.0
        return ur - 2*cr/gr1*(1 - ((p + pinfr)/(pr + pinfr))**(gr1/(2.0*gammar)))

    def plot_function(rhol,ul,pl,rhor,ur,pr,gammal,pinfl,gammar,pinfr,
                      xmin,xmax,ymin,ymax,show_phys,show_unphys):
        "Subfunction required for interactive (function of only interactive parameters)."
        ql = [rhol, ul, pl]
        qr = [rhor, ur, pr]
        paramsl = [gammal, pinfl]
        paramsr = [gammar, pinfr]

        hugoloc1 = lambda p: hugoniot_locus_1(p,ql,paramsl)
        hugoloc3 = lambda p: hugoniot_locus_3(p,qr,paramsr)
        intcurv1 = lambda p: integral_curve_1(p,ql,paramsl)
        intcurv3 = lambda p: integral_curve_3(p,qr,paramsr)

        def phi_l(p):
            "Check whether the 1-wave is a shock or rarefaction."
            if p >= pl:
                return hugoloc1(p)
            else:
                return intcurv1(p)

        def phi_r(p):
            "Check whether the 3-wave is a shock or rarefaction."
            if p >= pr:
                return hugoloc3(p)
            else:
                return intcurv3(p)

        phi = lambda p: phi_l(p)-phi_r(p)

        # Use fsolve to find p_star such that Phi(p_star)=0
        p0 = (ql[2] + qr[2])/2.0  # initial guess is the average of initial pressures
        p_star, info, ier, msg = fsolve(phi, p0, full_output=True, xtol=1.e-14)
        # For strong rarefactions, sometimes fsolve needs help
        if ier != 1:
            p_star, info, ier, msg = fsolve(phi, p0, full_output=True, factor=0.1, xtol=1.e-10)
            # This should not happen:
            if ier != 1:
                print('Warning: fsolve did not converge.')
        u_star = 0.5*(phi_l(p_star) + phi_r(p_star))

        # Set plot bounds
        fig, ax = plt.subplots(figsize=(12,4))
        x = (ql[2], qr[2], p_star)
        y = (ql[1], qr[1], u_star)
        dx, dy = xmax - xmin, ymax - ymin
        ax.set_xlim(min(0.00000001,xmin),xmax)
        ax.set_ylim(ymin,ymax)
        ax.set_xlabel('Pressure (p)', fontsize=15)
        ax.set_ylabel('Velocity (u)', fontsize=15)

        p = np.linspace(xmin,xmax,500)
        p1_shk = p[p>=pl]
        p1_rar = p[p<pl]
        p3_shk = p[p>=pr]
        p3_rar = p[p<pr]

        # Plot unphysical solutions
        if show_unphys:
            ax.plot(p1_rar,hugoloc1(p1_rar),'--r')
            ax.plot(p3_rar,hugoloc3(p3_rar),'--r')
            ax.plot(p1_shk,intcurv1(p1_shk),'--b')
            ax.plot(p3_shk,intcurv3(p3_shk),'--b')

        # Plot physical solutions
        if show_phys:
            ax.plot(p1_shk,hugoloc1(p1_shk),'-r')
            ax.plot(p3_shk,hugoloc3(p3_shk),'-r')
            ax.plot(p1_rar,intcurv1(p1_rar),'-b')
            ax.plot(p3_rar,intcurv3(p3_rar),'-b')
            if (p_star <= xmax and u_star > ymin and u_star < ymax):
                ax.plot(p_star, u_star, '-ok', markersize=10)
                ax.text(x[2] + 0.025*dx,y[2] + 0.025*dy, '$q_m$', fontsize=15)

        # Plot initial states and markers
        ax.plot(ql[2], ql[1], '-ok', markersize=10)
        ax.plot(qr[2], qr[1], '-ok', markersize=10)
        for i,label in enumerate(('$q_l$', '$q_r$')):
            ax.text(x[i] + 0.025*dx,y[i] + 0.025*dy,label, fontsize=15)
        plt.show()
    return plot_function


def euler_tammann_interactive_phase_plane(ql=(600.0, 10.0, 50000.0),
                                          qr=(50.0, -10.0, 25000.0),
                                          paramsl=(1.4, 0.0),
                                          paramsr=(7.0, 100.0)):
    "Create the GUI and output the interact app."
    # Create plot function for interact
    pp_plot = euler_tammann_phase_plane_plot()

    # Declare all widget sliders
    ql1_widget = widgets.FloatSlider(value=ql[0],min=0.01,max=1000.0, description=r'$\rho_l$')
    ql2_widget = widgets.FloatSlider(value=ql[1],min=-15,max=15.0, description='$u_l$')
    ql3_widget = widgets.FloatSlider(value=ql[2],min=1,max=200000.0, description='$p_l$')
    qr1_widget = widgets.FloatSlider(value=qr[0],min=0.01,max=1000.0, description=r'$\rho_r$')
    qr2_widget = widgets.FloatSlider(value=qr[1],min=-15,max=15.0, description='$u_r$')
    qr3_widget = widgets.FloatSlider(value=qr[2],min=1,max=200000.0, description='$p_r$')
    gamml_widget = widgets.FloatSlider(value=paramsl[0],min=0.01,max=10.0, description='$\gamma_l$')
    gammr_widget = widgets.FloatSlider(value=paramsr[0],min=0.01,max=10.0, description='$\gamma_r$')
    pinfl_widget = widgets.FloatSlider(value=paramsl[1],min=0.0,max=300000.0, description='$p_{\infty l}$')
    pinfr_widget = widgets.FloatSlider(value=paramsr[1],min=0.0,max=300000.0, description='$p_{\infty r}$')
    xmin_widget = widgets.BoundedFloatText(value=0.0000001, description='$p_{min}:$')
    xmax_widget = widgets.FloatText(value=200000, description='$p_{max}:$')
    ymin_widget = widgets.FloatText(value=-15, description='$u_{min}:$')
    ymax_widget = widgets.FloatText(value=15, description='$u_{max}:$')
    show_physical = widgets.Checkbox(value=True, description='Physical solution')
    show_unphysical = widgets.Checkbox(value=True, description='Unphysical solution')
    # Additional control widgets not called by function
    rhomax_widget = widgets.FloatText(value=1000, description=r'$\rho_{max}$')
    gammax_widget = widgets.FloatText(value=10, description='$\gamma_{max}$')
    pinfmax_widget = widgets.FloatText(value=300000, description='$p_{\infty max}$')

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
        ql1_widget.max = rhomax_widget.value
        qr1_widget.max = rhomax_widget.value
    def update_gammax(*args):
        gamml_widget.max = gammax_widget.value
        gammr_widget.max = gammax_widget.value
    def update_pinfmax(*args):
        pinfl_widget.max = pinfmax_widget.value
        pinfr_widget.max = pinfmax_widget.value
    xmin_widget.observe(update_xmin, 'value')
    xmax_widget.observe(update_xmax, 'value')
    ymin_widget.observe(update_ymin, 'value')
    ymax_widget.observe(update_ymax, 'value')
    rhomax_widget.observe(update_rhomax, 'value')
    gammax_widget.observe(update_gammax, 'value')
    pinfmax_widget.observe(update_pinfmax, 'value')

    # Organize slider widgets into boxes
    qleftright = widgets.VBox([widgets.HBox([ql1_widget, ql2_widget, ql3_widget]),
                              widgets.HBox([qr1_widget, qr2_widget, qr3_widget])])
    params = widgets.HBox([widgets.VBox([gamml_widget, gammr_widget]),
                           widgets.VBox([pinfl_widget, pinfr_widget])])
    plot_opts = widgets.HBox([widgets.VBox([show_physical, xmin_widget, ymin_widget]),
                              widgets.VBox([show_unphysical, xmax_widget, ymax_widget]),
                              widgets.VBox([rhomax_widget, gammax_widget, pinfmax_widget])])

    # Set up interactive GUI (tab style)
    interact_gui = widgets.Tab(children=[qleftright, params, plot_opts])
    interact_gui.set_title(0, 'Left and right states')
    interact_gui.set_title(1, 'Tammann EOS')
    interact_gui.set_title(2, 'Plot options')

    # Define interactive widget and run GUI
    ppwidget = interact(pp_plot, rhol=ql1_widget, ul=ql2_widget, pl=ql3_widget,
                          rhor=qr1_widget, ur=qr2_widget, pr=qr3_widget,
                          gammal=gamml_widget, pinfl=pinfl_widget,
                          gammar=gammr_widget, pinfr=pinfr_widget,
                          xmin=xmin_widget, xmax=xmax_widget,
                          ymin=ymin_widget, ymax=ymax_widget,
                          show_phys=show_physical, show_unphys=show_unphysical)
    ppwidget.widget.close()
    display(interact_gui)
    display(ppwidget.widget.out)
    

