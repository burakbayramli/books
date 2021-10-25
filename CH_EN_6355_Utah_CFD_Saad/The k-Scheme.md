# Advection using the k-Scheme
## CH EN 6355 - Computational Fluid Dynamics
**Prof. Tony Saad (<a>www.tsaad.net</a>) <br/>
slides at: <a>www.tsaad.net</a><br/>
Department of Chemical Engineering <br/>
University of Utah**
<hr/>

Here, we will implement the k-scheme or kappa-schemes for advection. It is easiest to implement this scheme since for different values of k, we recover all sorts of high-order flux approximations. We will assume a positive advecting velocity for illustration purposes.

We are solving the constant speed advection equation given by
\begin{equation}
u_t = - c u_x = - F_x;\quad F = cu
\end{equation}
We will use a simple Forward Euler explicit method. Using a finite volume integration, we get
\begin{equation}
u_i^{n+1} = u_i^n - \frac{\Delta t}{\Delta x} (F_{i+\tfrac{1}{2}}^n - F_{i-\tfrac{1}{2}}^n)
\end{equation}
For constant grid spacing, the k-Scheme is given by
\begin{equation}
{\phi _f} = {\phi _{\rm{C}}} + \frac{{1 - k}}{4}({\phi _{\rm{C}}} - {\phi _{\rm{U}}}) + \frac{{1 + k}}{4}({\phi _{\rm{D}}} - {\phi _{\rm{C}}})
\end{equation}
which, for a positive advecting velocity, gives us
\begin{equation}
F_{i + {\textstyle{1 \over 2}}}^n = c\phi _{i + {\textstyle{1 \over 2}}}^n = c{\phi _i} + c\frac{{1 - k}}{4}({\phi _i} - {\phi _{i - 1}}) + c\frac{{1 + k}}{4}({\phi _{i + 1}} - {\phi _i})
\end{equation}


```python
import numpy as np
%matplotlib inline
%config InlineBackend.figure_format = 'svg'
import matplotlib.pyplot as plt
import matplotlib.animation as animation
plt.rcParams['animation.html'] = 'html5'
from matplotlib import cm
```


```python
def step(x,x0):
    x0 = 0.6
    x1 = 0.8
    result = x - x0
    result[x-x1<x1] = 1.0            
    result[x<x0] = 0.0
    result[x>x1] = 0.0  
    return result

def gaussian(x,x0):
    s = 0.08
    s = s*s
    result = np.exp( -(x-x0)**2/s)
    return result
```


```python
L = 1.0
n = 128 # cells
dx = L/n # n intervals
x = np.linspace(-3*dx/2, L + 3*dx/2, n+4) # include ghost cells - we will include 2 ghost cells on each side for high order schemes

# create arrays
phi = np.zeros(n+4) # cell centered quantity
f = np.zeros(n+4+1) # flux
u = np.ones(n+4+1) # velocity field - assumed to live on faces same as flux

x0 = 0.3
# u0 = np.zeros(N + 2)
# u0[1:-1] = np.sin(2*np.pi*x)
# u0 = np.zeros(N)
# phi0 = np.sin(np.pi*x)
phi0 = gaussian(x,x0) + step(x,x0)
# u0 = triangle(x,0.5,0.75,1)
# u0[0:N//2] = 1.0
plt.plot(x,phi0)
```




    [<matplotlib.lines.Line2D at 0x1199f8630>]




    
![svg](output_4_1.svg)
    



```python
cfl =0.5
c = 1.0 # use a negative value for left traveling waves
dt = cfl*dx/abs(c)
print('dt=',dt)
print('dx=',dx)
```

    dt= 0.00390625
    dx= 0.0078125



```python
# the k scheme
k = 0.5
# finite volume implementation with arrays for fluxes
t = 0
tend= L/abs(c)

sol = []
sol.append(phi0)
ims = []

fig = plt.figure(figsize=[5,3],dpi=200)
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.size"] = 10
plt.rc('text', usetex=True)

# plt.grid()
plt.xlim([0.,L])
plt.ylim([-0.25,1.25])
plt.xlabel('$x$')
plt.ylabel('$\phi$')
plt.tight_layout()
# plot initial condition
plt.plot(x,phi0,'darkred',animated=True)

i = 0
while t < tend:    
    phin = sol[-1]

#     if (i%16==0):
#         shift = int(np.ceil(c*(t-dt)/dx))
#         im = plt.plot(x[2:-2], np.roll(phin[2:-2], -shift) ,'k-o',markevery=2,markersize=3.5,markerfacecolor='deepskyblue',
#              markeredgewidth=0.25, markeredgecolor='k',linewidth=0.45, animated=True)
#         ims.append(im)
    
    # impose periodic conditions
    phin[-2] = phin[2]
    phin[-1] = phin[3]    
    phin[0] = phin[-4]        
    phin[1] = phin[-3]            
    
    phi = np.zeros_like(phi0)
    
    # predictor - take half a step and use upwind
    # du/dt = -c*du/dx
    if c >= 0:
        ϕc = phin[1:-2] # phi upwind
    else:
        ϕc = phin[2:-1] # phi upwind
    
    f[2:-2] = c*ϕc
    phi[2:-2] = phin[2:-2] - dt/2.0/dx*(f[3:-2] - f[2:-3])
    phi[-2] = phi[2]
    phi[-1] = phi[3]    
    phi[0] = phi[-4]        
    phi[1] = phi[-3]                
    
    # du/dt = -c*du/dx
    if c >= 0:
        ϕc = phi[1:-2] # phi upwind
        ϕu = phi[:-3]  # phi far upwind
        ϕd = phi[2:-1] # phi downwind
    else:
        ϕc = phi[2:-1] # phi upwind
        ϕu = phi[3:]  # phi far upwind
        ϕd = phi[1:-2] # phi downwind
        
    f[2:-2] = ϕc + (1-k)/4.0*(ϕc - ϕu) + (1+k)/4.0*(ϕd - ϕc)
    f = c*f # multiply the flux by the velocity
    # advect
    phi[2:-2] = phin[2:-2] - c * dt/dx*(f[3:-2] - f[2:-3]) #+ dt/dx/dx*diffusion
    t += dt    
    i+=1
    sol.append(phi)


# plt.annotate('k = '+ str(k), xy=(0.5, 0.8), xytext=(0.015, 0.9),fontsize=8)
# plt.legend(('exact','numerical'),loc='upper left',fontsize=7)
# ani = animation.ArtistAnimation(fig, ims, interval=100, blit=True,
#                                 repeat_delay=1000)

# ani.save('k-scheme-'+str(k)+'.mp4',dpi=300,fps=24)
```


    
![svg](output_6_0.svg)
    



```python
plt.plot(sol[0], label='initial condition')
plt.plot(sol[-1], label='one residence time')
plt.legend()
plt.grid()
```


    
![svg](output_7_0.svg)
    


# Create Animation in Moving Reference Frame


```python
"""
Create Animation in Moving Reference Frame
"""
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
matplotlib.use("Agg")
fig, ax = plt.subplots(figsize=(4,3),dpi=150) 
ax.grid(True)
f0 = sol[0]
line0, = ax.plot(x[2:-2], f0[2:-2] ,'r-',linewidth=0.75, animated=True)
line1, = ax.plot(x[2:-2], f0[2:-2] ,'k-o',markevery=2,markersize=3.5,markerfacecolor='deepskyblue',
             markeredgewidth=0.25, markeredgecolor='k',linewidth=0.45, animated=True)

ann = ax.annotate('time ='+str(round(t,3))+' s', xy=(2, 1), xytext=(40, 200),xycoords='figure points')
ax.annotate('k ='+str(k) + ' (k-scheme)', xy=(2, 1), xytext=(40, 190),xycoords='figure points')
plt.tight_layout()


def animate_moving(i):
    print('time=',i*dt)
    t = i*dt
    xt = x + i*1.1*c*dt
    line0.set_xdata(xt[2:-2])
    line1.set_xdata(xt[2:-2])    
    ax.axes.set_xlim(xt[0],0.0*dx + xt[-1])
    f = sol[i]
    ax.axes.set_ylim(1.1*min(f) - 0.1,1.1*max(f))
    ann.set_text('time ='+str(round(t,4))+'s (' + str(i)+ ').')
    shift =int(np.ceil(i*c*dt/dx))
    line1.set_ydata(np.roll(f[2:-2], -shift))

    f0 = sol[0]
    line0.set_ydata(f0[2:-2])
    return line0,line1


# Init only required for blitting to give a clean slate.
def init():
    line0.set_ydata(np.ma.array(x[2:-2], mask=True))
    line1.set_ydata(np.ma.array(x[2:-2], mask=True))    
    return line0,line1

ani = animation.FuncAnimation(fig, animate_moving, np.arange(0,len(sol),2*int(1/cfl)), init_func=init,
                              interval=20, blit=False)
print('done!')
```

    done!



    
![svg](output_9_1.svg)
    



```python
ani.save('__k-scheme_' + str(k)+'.mp4',fps=24,dpi=300)
```

    time= 0.0
    time= 0.0078125
    time= 0.015625
    time= 0.0234375
    time= 0.03125
    time= 0.0390625
    time= 0.046875
    time= 0.0546875
    time= 0.0625
    time= 0.0703125
    time= 0.078125
    time= 0.0859375
    time= 0.09375
    time= 0.1015625
    time= 0.109375
    time= 0.1171875
    time= 0.125
    time= 0.1328125
    time= 0.140625
    time= 0.1484375
    time= 0.15625
    time= 0.1640625
    time= 0.171875



    ---------------------------------------------------------------------------

    KeyboardInterrupt                         Traceback (most recent call last)

    <ipython-input-29-f14b34654064> in <module>()
    ----> 1 ani.save('__k-scheme_' + str(k)+'.mp4',fps=24,dpi=300)
    

    /anaconda3/lib/python3.6/site-packages/matplotlib/animation.py in save(self, filename, writer, fps, dpi, codec, bitrate, extra_args, metadata, extra_anim, savefig_kwargs)
       1172                         # TODO: See if turning off blit is really necessary
       1173                         anim._draw_next_frame(d, blit=False)
    -> 1174                     writer.grab_frame(**savefig_kwargs)
       1175 
       1176         # Reconnect signal for first draw if necessary


    /anaconda3/lib/python3.6/site-packages/matplotlib/animation.py in grab_frame(self, **savefig_kwargs)
        374             # frame format and dpi.
        375             self.fig.savefig(self._frame_sink(), format=self.frame_format,
    --> 376                              dpi=self.dpi, **savefig_kwargs)
        377         except (RuntimeError, IOError) as e:
        378             out, err = self._proc.communicate()


    /anaconda3/lib/python3.6/site-packages/matplotlib/figure.py in savefig(self, fname, frameon, transparent, **kwargs)
       2092             self.set_frameon(frameon)
       2093 
    -> 2094         self.canvas.print_figure(fname, **kwargs)
       2095 
       2096         if frameon:


    /anaconda3/lib/python3.6/site-packages/matplotlib/backend_bases.py in print_figure(self, filename, dpi, facecolor, edgecolor, orientation, format, bbox_inches, **kwargs)
       2073                     orientation=orientation,
       2074                     bbox_inches_restore=_bbox_inches_restore,
    -> 2075                     **kwargs)
       2076             finally:
       2077                 if bbox_inches and restore_bbox:


    /anaconda3/lib/python3.6/site-packages/matplotlib/backends/backend_agg.py in print_raw(self, filename_or_obj, *args, **kwargs)
        459 
        460     def print_raw(self, filename_or_obj, *args, **kwargs):
    --> 461         FigureCanvasAgg.draw(self)
        462         renderer = self.get_renderer()
        463         with cbook._setattr_cm(renderer, dpi=self.figure.dpi), \


    /anaconda3/lib/python3.6/site-packages/matplotlib/backends/backend_agg.py in draw(self)
        400         toolbar = self.toolbar
        401         try:
    --> 402             self.figure.draw(self.renderer)
        403             # A GUI class may be need to update a window using this draw, so
        404             # don't forget to call the superclass.


    /anaconda3/lib/python3.6/site-packages/matplotlib/artist.py in draw_wrapper(artist, renderer, *args, **kwargs)
         48                 renderer.start_filter()
         49 
    ---> 50             return draw(artist, renderer, *args, **kwargs)
         51         finally:
         52             if artist.get_agg_filter() is not None:


    /anaconda3/lib/python3.6/site-packages/matplotlib/figure.py in draw(self, renderer)
       1647 
       1648             mimage._draw_list_compositing_images(
    -> 1649                 renderer, self, artists, self.suppressComposite)
       1650 
       1651             renderer.close_group('figure')


    /anaconda3/lib/python3.6/site-packages/matplotlib/image.py in _draw_list_compositing_images(renderer, parent, artists, suppress_composite)
        136     if not_composite or not has_images:
        137         for a in artists:
    --> 138             a.draw(renderer)
        139     else:
        140         # Composite any adjacent images together


    /anaconda3/lib/python3.6/site-packages/matplotlib/artist.py in draw_wrapper(artist, renderer, *args, **kwargs)
         48                 renderer.start_filter()
         49 
    ---> 50             return draw(artist, renderer, *args, **kwargs)
         51         finally:
         52             if artist.get_agg_filter() is not None:


    /anaconda3/lib/python3.6/site-packages/matplotlib/axes/_base.py in draw(self, renderer, inframe)
       2626             renderer.stop_rasterizing()
       2627 
    -> 2628         mimage._draw_list_compositing_images(renderer, self, artists)
       2629 
       2630         renderer.close_group('axes')


    /anaconda3/lib/python3.6/site-packages/matplotlib/image.py in _draw_list_compositing_images(renderer, parent, artists, suppress_composite)
        136     if not_composite or not has_images:
        137         for a in artists:
    --> 138             a.draw(renderer)
        139     else:
        140         # Composite any adjacent images together


    /anaconda3/lib/python3.6/site-packages/matplotlib/artist.py in draw_wrapper(artist, renderer, *args, **kwargs)
         48                 renderer.start_filter()
         49 
    ---> 50             return draw(artist, renderer, *args, **kwargs)
         51         finally:
         52             if artist.get_agg_filter() is not None:


    /anaconda3/lib/python3.6/site-packages/matplotlib/text.py in draw(self, renderer)
       2391         # Draw text, including FancyBboxPatch, after FancyArrowPatch.
       2392         # Otherwise, a wedge arrowstyle can land partly on top of the Bbox.
    -> 2393         Text.draw(self, renderer)
       2394 
       2395     def get_window_extent(self, renderer=None):


    /anaconda3/lib/python3.6/site-packages/matplotlib/artist.py in draw_wrapper(artist, renderer, *args, **kwargs)
         48                 renderer.start_filter()
         49 
    ---> 50             return draw(artist, renderer, *args, **kwargs)
         51         finally:
         52             if artist.get_agg_filter() is not None:


    /anaconda3/lib/python3.6/site-packages/matplotlib/text.py in draw(self, renderer)
        752                     textrenderer.draw_tex(gc, x, y, clean_line,
        753                                           textobj._fontproperties, angle,
    --> 754                                           mtext=mtext)
        755                 else:
        756                     textrenderer.draw_text(gc, x, y, clean_line,


    /anaconda3/lib/python3.6/site-packages/matplotlib/backends/backend_agg.py in draw_tex(self, gc, x, y, s, prop, angle, ismath, mtext)
        231         texmanager = self.get_texmanager()
        232 
    --> 233         Z = texmanager.get_grey(s, size, self.dpi)
        234         Z = np.array(Z * 255.0, np.uint8)
        235 


    /anaconda3/lib/python3.6/site-packages/matplotlib/texmanager.py in get_grey(self, tex, fontsize, dpi)
        418         alpha = self.grey_arrayd.get(key)
        419         if alpha is None:
    --> 420             pngfile = self.make_png(tex, fontsize, dpi)
        421             X = _png.read_png(os.path.join(self.texcache, pngfile))
        422             self.grey_arrayd[key] = alpha = X[:, :, -1]


    /anaconda3/lib/python3.6/site-packages/matplotlib/texmanager.py in make_png(self, tex, fontsize, dpi)
        383             self._run_checked_subprocess(
        384                 ["dvipng", "-bg", "Transparent", "-D", str(dpi),
    --> 385                  "-T", "tight", "-o", pngfile, dvifile], tex)
        386         return pngfile
        387 


    /anaconda3/lib/python3.6/site-packages/matplotlib/texmanager.py in _run_checked_subprocess(self, command, tex)
        296             report = subprocess.check_output(command,
        297                                              cwd=self.texcache,
    --> 298                                              stderr=subprocess.STDOUT)
        299         except subprocess.CalledProcessError as exc:
        300             raise RuntimeError(


    /anaconda3/lib/python3.6/subprocess.py in check_output(timeout, *popenargs, **kwargs)
        354 
        355     return run(*popenargs, stdout=PIPE, timeout=timeout, check=True,
    --> 356                **kwargs).stdout
        357 
        358 


    /anaconda3/lib/python3.6/subprocess.py in run(input, timeout, check, *popenargs, **kwargs)
        423     with Popen(*popenargs, **kwargs) as process:
        424         try:
    --> 425             stdout, stderr = process.communicate(input, timeout=timeout)
        426         except TimeoutExpired:
        427             process.kill()


    /anaconda3/lib/python3.6/subprocess.py in communicate(self, input, timeout)
        848                 self._stdin_write(input)
        849             elif self.stdout:
    --> 850                 stdout = self.stdout.read()
        851                 self.stdout.close()
        852             elif self.stderr:


    KeyboardInterrupt: 



```python
import urllib
import requests
from IPython.core.display import HTML
def css_styling():
    styles = requests.get("https://raw.githubusercontent.com/saadtony/NumericalMethods/master/styles/custom.css")
    return HTML(styles.text)
css_styling()
```




CSS style adapted from https://github.com/barbagroup/CFDPython. Copyright (c) Barba group
<link href='http://fonts.googleapis.com/css?family=Merriweather' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Bitter' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Oxygen' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Lora' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Fenix' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Alegreya+Sans:100,300,400,500,700,800,900,100italic,300italic,400italic,500italic,700italic,800italic,900italic' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:300,400' rel='stylesheet' type='text/css'>

<style>
    @font-face {
        font-family: "Computer Modern";
        src: url('http://mirrors.ctan.org/fonts/cm-unicode/fonts/otf/cmunss.otf');
    }

    /*div.cell{
        width:800px;
        margin-left:16% !important;
        margin-right:auto;
    } */

    /* set the font size in tables */
    tr, td, th{
        font-size:110%;
    }

    /* spec for headers */
    h1 {
        font-family: 'Bitter', serif;
    }

    h2 {
        font-family: 'Fenix', serif;
    }

    h3{
        font-family: 'Fenix', serif;
        margin-top:12px;
        margin-bottom: 3px;
    }

    h4{
        font-family: 'Fenix', serif;
    }

    h5 {
        font-family: 'Alegreya Sans', sans-serif;
    }

    div.text_cell_render{
        font-family: 'Merriweather','Alegreya Sans','Lora', 'Oxygen', "Helvetica Neue", Arial, Helvetica, Geneva, sans-serif;
        line-height: 160%;
        font-size: 130%;
    }

    .CodeMirror{
        font-family: "Source Code Pro";
        font-size: 100%;
    }

    .text_cell_render h1 {
        font-weight: 200;
        font-size: 32pt;
        line-height: 120%;
        color:#CD2305;
        margin-bottom: 0.5em;
        margin-top: 0.5em;
        display: block;
    }

    .text_cell_render h2 {
        font-size: 26pt;
        text-align: center;
    }

    .text_cell_render h3 {
        font-size: 20pt;
    }

    .text_cell_render h4 {
        font-size: 18pt;
    }

    .text_cell_render h5 {
        font-weight: 300;
        font-size: 16pt;
        color: #CD2305;
        font-style: italic;
        margin-bottom: .5em;
        margin-top: 0.5em;
        display: block;
    }

    .warning{
        color: rgb( 240, 20, 20 )
    }  

/*  div#notebook {background-color: #1e1e1e; border-top: none;}
    div#notebook-container {background-color: rgb(180, 180, 180);}
 */

</style>

<script>
    MathJax.Hub.Config({
                TeX: {
                    extensions: ["AMSmath.js"]
                },
                tex2jax: {
                    inlineMath: [ ['$','$'], ["\\(","\\)"] ],
                    displayMath: [ ['$$','$$'], ["\\[","\\]"] ]
                },
                displayAlign: 'center', // Change this to 'center' to center equations.
                "HTML-CSS": {
                    availableFonts: ["TeX"],
                    scale: 100,
                    styles: {'.MathJax_Display': {"margin": 4}}
                }
        });
</script>





```python

```
