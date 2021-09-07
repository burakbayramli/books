import sys, os
from IPython.display import display
from ipywidgets import widgets
from ipywidgets import interact

def plot_with_stripes(rho_l=3.,u_l=0.,p_l=3.,
                      rho_r=1.,u_r=0.,p_r=1.,gamma=1.4,t=0.4):  
    import matplotlib.pyplot as plt
    import numpy as np
    from exact_solvers import euler  
    from utils import riemann_tools
    q_l = euler.primitive_to_conservative(rho_l,u_l,p_l)
    q_r = euler.primitive_to_conservative(rho_r,u_r,p_r)
        
    x = np.linspace(-1.,1.,1000)
    states, speeds, reval, wave_types = euler.exact_riemann_solution(q_l, q_r, gamma=gamma)
    if t == 0:
        q = np.zeros((3,len(x)))
        q[0,:] = q_l[0]*(x<=0) + q_r[0]*(x>0)
        q[1,:] = q_l[1]*(x<=0) + q_r[1]*(x>0)
        q[1,:] = q_l[2]*(x<=0) + q_r[2]*(x>0)
    else:
        q = reval(x/t)
    primitive = euler.conservative_to_primitive(q[0],q[1],q[2])
    
    # compute particle trajectories:
    def reval_rho_u(x): 
        eps = 1.e-16
        q = reval(x)
        rho = q[0]
        u = q[1]/(q[0]+eps)
        rho_u = np.vstack((rho,u))
        return rho_u
    
    # Specify density of trajectories to left and right:
    num_left = 10
    num_right = 10
    rho_left = q_l[0] / 10.
    rho_right = q_r[0] / 10.
    x_traj, t_traj, xmax = riemann_tools.compute_riemann_trajectories(states, speeds, reval_rho_u, wave_types,
                                  i_vel=1, xmax=1, rho_left=rho_left, rho_right=rho_right)
                                                                          
    fig = plt.figure(figsize=(9,8))
    names = ['Density','Velocity','Pressure']
    axes = [0]*3
    for i in range(3):
        axes[i] = fig.add_subplot(3,1,i+1)
        q = primitive[i]
        plt.plot(x,q,'-k',linewidth=3)
        plt.title(names[i])
        qmax = max(q)
        qmin = min(q)
        qdiff = qmax - qmin
        if qdiff == 0: qdiff = qmin*0.5
        axes[i].set_ylim((qmin-0.1*qdiff,qmax+0.1*qdiff))
        axes[i].set_xlim(-xmax,xmax)
                
        if i==0:
            # plot stripes only on density plot
            n = np.array([j for j,v in enumerate(t > t_traj) if v])
            if len(n)==0:
                n = 0
            else:
                n = min(n.max(), len(t_traj)-1)

            for i in range(1, x_traj.shape[1]-1):
                j1 = np.array([j for j,v in enumerate(x_traj[n,i] > x) if v])
                if len(j1)==0:
                    j1 = 0
                else:
                    j1 = min(j1.max(), len(x)-1)
                j2 = np.array([j for j,v in enumerate(x_traj[n,i+1] > x) if v])
                if len(j2)==0:
                    j2 = 0
                else:
                    j2 = min(j2.max(), len(x)-1)

                # set advected color for density plot:
                if x_traj[0,i]<0: 
                    # shades of red for fluid starting from x<0
                    if np.mod(i,2)==0:
                        c = [1,0,0]
                    else:
                        c = [1,0.8,0.8]
                else:
                    # shades of blue for fluid starting from x<0
                    if np.mod(i,2)==0:
                        c = [0,0,1]
                    else:
                        c = [0.8,0.8,1]
                plt.fill_between(x[j1:j2],q[j1:j2],0,color=c)
    plt.tight_layout()
    plt.show()
 
def euler_demo1(rho_l=2.,u_l=0.,p_l=2.5,
                rho_r=3.,u_r=0.,p_r=5., gamma=1.4):
    
    rhol_widget=widgets.FloatSlider(min=1.,max=10.,step=0.1,value=rho_l,description=r'$\rho_l$')
    ul_widget=widgets.FloatSlider(min=-10.,max=10.,step=0.1,value=u_l,description=r'$u_l$')
    pl_widget=widgets.FloatSlider(min=1.,max=10.,step=0.1,value=p_l,description=r'$p_l$')
    rhor_widget=widgets.FloatSlider(min=1.,max=10.,step=0.1,value=rho_r,description=r'$\rho_r$')
    ur_widget=widgets.FloatSlider(min=-10.,max=10.,step=0.1,value=u_r,description=r'$u_r$')
    pr_widget=widgets.FloatSlider(min=1.,max=10.,step=0.1,value=p_r,description=r'$p_r$')
    gamma_widget=widgets.FloatSlider(min=1.1,max=2.,step=0.1,value=gamma,description=r'$\gamma$')
    t_widget=widgets.FloatSlider(min=0.,max=1.,step=0.1,value=0.5)

    interact_gui = widgets.VBox(\
         [widgets.HBox([rhol_widget, rhor_widget, gamma_widget]),
          widgets.HBox([ul_widget, ur_widget, t_widget]),
          widgets.HBox([pl_widget, pr_widget])]);

    mainwidget = interact(plot_with_stripes,
                          rho_l=rhol_widget,u_l=ul_widget,p_l=pl_widget,
                          rho_r=rhor_widget,u_r=ur_widget,p_r=pr_widget,
                          gamma=gamma_widget,t=t_widget);

    try:
        mainwidget.widget.close()
        display(interact_gui)
        display(mainwidget.widget.out)
    except:
        pass
