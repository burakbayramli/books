import numpy as np
import matplotlib.pyplot as plt
import matplotlib

def speed(q, xi):
    return q

def exact_riemann_solution(q_l,q_r):
    print (q_l,q_r)
    f = lambda q: 0.5*q*q
    states = np.array([[q_l, q_r]])
    if q_l > q_r:  # Shock wave
        shock_speed = (f(q_l)-f(q_r))/(q_l-q_r)
        speeds = [shock_speed]
        wave_types = ['shock']
        def reval(xi):
            q = np.zeros((1,len(xi)))
            q[0,:] = (xi < shock_speed)*q_l \
              + (xi >=shock_speed)*q_r
            return q

    else:  # Rarefaction wave
        c_l  = q_l
        c_r = q_r

        speeds = [[c_l,c_r]]
        wave_types = ['rarefaction']

        def reval(xi):
            q = np.zeros((1,len(xi)))
            q[0,:] = (xi<=c_l)*q_l \
              + (xi>=c_r)*q_r \
              + (c_l<xi)*(xi<c_r)*xi
            return q

    return states, speeds, reval, wave_types
    
def rarefaction():
    q_l, q_r = 2.0, 4.0
    states, speeds, reval, wave_type = exact_riemann_solution(q_l ,q_r)

    plot_function = make_plot_function(states, speeds, reval, wave_type, 
                                       layout='horizontal',
                                       variable_names=['q'],
                                       plot_chars=[speed])
    return plot_function


def make_plot_function(states_list, speeds_list, riemann_eval_list,
                       wave_types_list=None, names=None, layout='horizontal',
                       variable_names=None, colors=('multi', 'green', 'orange'),
                       plot_chars=None, derived_variables=None, aux=None, contact_index=None,
                       vertical_spacing=0, show_time_legend=False):
    if type(states_list) is not list:
        states_list = [states_list]
        speeds_list = [speeds_list]
        riemann_eval_list = [riemann_eval_list]
        if wave_types_list is not None:
            wave_types_list = [wave_types_list]

    if wave_types_list is None:
        wave_types_list= [['contact']*len(speeds_list[0])]*len(speeds_list)

    num_eqn = states_list[0].shape[0]

    num_axes = num_eqn + 1

    def plot_function(t, which_char=None):
        fig_width = 9
        fig, ax = plt.subplots(1,num_axes,figsize=(fig_width, 3))

        for i in range(len(states_list)):
            states = states_list[i]
            speeds = speeds_list[i]
            riemann_eval = riemann_eval_list[i]
            wave_types = wave_types_list[i]

            plot_riemann(states, speeds, riemann_eval, wave_types, t, ax,
                         colors[i], layout=layout,
                         variable_names=variable_names, t_pointer=False,
                         derived_variables=derived_variables)

            if names is not None and show_time_legend:
                # We could use fig.legend here if we had the line plot handles
                ax[1].legend(names,loc='best', title='time = %.2f' % t)
            if names is not None and not show_time_legend:
                ax[1].legend(names,loc='best')
            if names is None and show_time_legend:
                ax[1].legend([],loc='best', title='time = %.2f' % t)


        plt.show()
        return None

    def real_plot_function(t):
        plot_function(t,None)

    return real_plot_function


def plot_waves(states, s, riemann_eval, wave_types, t=0.1, ax=None,
               color='multi', t_pointer=False, xmax=None):

    if wave_types is None:
        wave_types = ['contact']*len(s)

    print ('t',t)    
    print ('s',s)    

    colors = {}
    if color == 'multi':
        colors['shock'] = 'r'
        colors['raref'] = 'b'
        colors['contact'] = 'k'
    else:
        colors['shock'] = color
        colors['raref'] = color
        colors['contact'] = color

    if ax is None:
        fig, ax = plt.subplots()

    tmax = 1.0
    if xmax is None: xmax = 0.
    print ('s',s)
    for i in range(len(s)):
        print ('here2')
        speeds = np.linspace(s[i][0],s[i][1],5)
        print ('speeds',speeds)
        for ss in speeds:
            x1 = tmax * ss
            print (x1)
            xmax = max(xmax,abs(x1))

    xmax = max(0.001, xmax)
    ax.set_xlim(-xmax,xmax)

    
def plot_riemann(states, s, riemann_eval, wave_types=None, t=0.1, ax=None,
                 color='multi', layout='horizontal', variable_names=None,
                 t_pointer=False, extra_axes=False, fill=(),
                 derived_variables=None, xmax=None):
    
    num_vars, num_states = states.shape
    pstates = states.copy()
    if derived_variables:
        num_vars = len(derived_variables(states[:,0]))
        for i in range(num_states):
            pstates[:,i] = derived_variables(states[:,i])

    if ax is not None:
        assert len(ax) == num_vars + 1 + extra_axes

    if wave_types is None:
        wave_types = ['contact']*len(s)

    if variable_names is None:
        if num_vars == 1:
            variable_names = ['q']
        else:
            variable_names = ['$q_%s$' % i for i in range(1,num_vars+1)]

    if ax is None:  # Set up a new plot and axes
        existing_plots = False
        num_axes = num_vars+1
        if extra_axes: num_axes += extra_axes
        # Plots side by side
        if num_axes >= 4:
            fig_width = 10
        else:
            fig_width = 3*num_axes
        fig, ax = plt.subplots(1,num_axes,figsize=(fig_width,3))
        plt.subplots_adjust(wspace=0.5)
        plt.tight_layout()
    else:
        assert len(ax) == num_vars + 1 + extra_axes
        existing_plots = True
        ylims = []
        for i in range(1,len(ax)):
            ylims.append(ax[i].get_ylim())

    # Make plot boundaries grey
    for axis in ax:
        for child in axis.get_children():
            if isinstance(child, matplotlib.spines.Spine):
                child.set_color('#dddddd')

    # Plot wave characteristics in x-t plane
    plot_waves(pstates, s, riemann_eval, wave_types, t=t, ax=ax[0], color=color,
               t_pointer=t_pointer, xmax=xmax)

    if xmax is None:
        xmax = ax[0].get_xlim()[1]

    # Plot conserved quantities as function of x for fixed t
    # Use xi values in [-10,10] unless the wave speeds are so large
    # that we need a larger range
    xi_range = np.linspace(min(-10, 2*np.min(s[0])), max(10, 2*np.max(s[-1])))
    q_sample = riemann_eval(xi_range)
    if derived_variables:
        q_sample = derived_variables(q_sample)

    for i in range(num_vars):
        # Set axis bounds to comfortably fit the values that will be plotted
        ax[i+1].set_xlim((-1,1))
        qmax = max(np.nanmax(q_sample[i][:]), np.nanmax(pstates[i,:]))
        qmin = min(np.nanmin(q_sample[i][:]), np.nanmin(pstates[i,:]))
        qdiff = qmax - qmin
        ax[i+1].set_xlim(-xmax, xmax)
        if qmin == qmax:
            qmin = qmin*0.9
            qmax = qmin*1.1+0.01
        if existing_plots:
            ax[i+1].set_ylim((min(ylims[i][0],qmin-0.1*qdiff), max(ylims[i][1],qmax+0.1*qdiff)))
        else:
            ax[i+1].set_ylim((qmin-0.1*qdiff, qmax+0.1*qdiff))

        ax[i+1].set_title(variable_names[i]+' at t = %6.3f' % t)

    x = np.linspace(-xmax, xmax, 1000)
    if t>0:
        # Make sure we have a value of x between each pair of waves
        # This is important e.g. for nearly-pressureless gas,
        # in order to catch small regions
        wavespeeds = []
        for speed in s:
            wavespeeds += convert_to_list(speed)

        wavespeeds = np.array(wavespeeds)
        xm = 0.5*(wavespeeds[1:]+wavespeeds[:-1])*t
        print ('xm',xm)
        iloc = np.searchsorted(x,xm)
        x = np.insert(x, iloc, xm)

    # Avoid dividing by zero
    q = riemann_eval(x/(t+1e-10))
    if derived_variables:
        q = derived_variables(q)

    for i in range(num_vars):
        if color == 'multi':
            ax[i+1].plot(x,q[i][:],'-k',lw=2)
        else:
            ax[i+1].plot(x,q[i][:],'-',color=color,lw=2)
        if i in fill:
            ax[i+1].fill_between(x,q[i][:],color='b')
            ax[i+1].set_ybound(lower=0)

    return ax
    
def convert_to_list(x):
    if isinstance(x, (list, tuple)):
        return x
    else:
        return [x]
   
res = rarefaction()
print (res(0.3))


