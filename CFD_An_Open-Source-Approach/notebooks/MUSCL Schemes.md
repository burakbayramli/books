## MUSCL Schemes




In this notebook, we will explore high-order finite volume via MUSCL reconstruction.

To run each of the following cells, use the keyboard shortcut **SHIFT** + **ENTER**, press the button ``Run`` in the toolbar or find the option ``Cell > Run Cells`` from the menu bar. For more shortcuts, see ``Help > Keyboard Shortcuts``.

To get started, import the required Python modules by running the cell below.


```python
# Configuration for visualizing the plots
%matplotlib notebook
%config InlineBackend.figure_format = 'retina'

# Required modules
import numpy as np
import matplotlib.pyplot as plt

# Import figure style and custom functions
import nbtools as nb
```

We wil implement finite-volume methods based on MUSCL reconstruction to solve 

\begin{align}
	\frac{\partial u}{\partial t} + \frac{\partial u}{\partial x} = 0,
\end{align}
on a grid $x\in[0,2]$ with periodic boundary conditions. 

Following the example in the textbook, we implement ``residual`` to compute the residual using the second-order upwind-biased method. For the advection scheme, we use a Riemann solver of the form $F(u_L,u_R) = F(u_L)$. 

**Note**: Only ``dl`` is implemented due to the upwind Riemann solver. Later, you will need to implement the right slope ``dr``.

Complete the functions below. For any three values of ``u``, ``u0``, ``u1`` and ``u2`` are ordered from left to right in ``get_slope``. For example, $u_{i-2}, u_{i-1}, u_i$.


```python
def get_slope(u0, u1, u2, b):
    # Linear reconstruction
    delta = # Complete definition for delta_i
    return delta

def residual(u, a, dx):
    # Initialize residual vector
    res = 0.0*u
    b = 0.0
    for i in range(n):
        # Interface i+1/2
        if i == n - 1:
            # Enforce periodic BC at x=L
            dl = get_slope(u[i - 1], u[i], u[0], b)
        else:
            dl = get_slope(u[i - 1], u[i], u[i + 1], b)
            
        # Compute u_i+1/2,L
        ul = 
        
        # Compute f_i+1/2=a*uL using the upwind Riemann solver
        fR = a*ul
        
        # Interface i-i/2
        dl = get_slope(u[i - 2], u[i - 1], u[i], b) 
        
        # Compute u_i-1/2,L
        ul = 
        
        # Computr f_i-1/2=a*uL using the upwind Rieman solver
        fL = 
        
        res[i] = -(fupw_R - fupw_L)/dx
    return res
```

To advance the solution in time, we provide the second-order midpoint method from the Time-Stepping Methods notebook.


```python
def advance_solution(ut, a, dx, dt, tf):
    t = 0
    while t < tf:
        r = residual(ut, a, dx)
        um = ut + 0.5*dt*r
        rm = residual(um, a, dx)
        uf = ut + 1.0*dt*rm
        ut = 1.0*uf
        t += dt 
    return ut
```

Complete the following cell to set the initial parameters of the problem. Consider
- ``a = 1.0``
- ``n = 300``
- ``L = 2.0``
- ``dt = 2e-3``
- ``tf = 2.0``
The initial solution profile is given by
\begin{align}
	u(x, 0) &= 
	\begin{cases}
		e^{-20(x-0.5)^2} & \text{if } x < 1.2, \\ 
		1 & \text{if } 1.2 < x < 1.5,	 \\
		0 & \text{otherwise}.
	\end{cases}
\end{align}


```python
a = 
n = 
L = 
dt = 
tf = 

# Calculate mesh spacing and grid points
dx = L/n 
x = np.arange(0, L, dx)

# Initialize the solution
u0 = np.zeros(n)
for i, xval in enumerate(x):
    if xval < 1.2:
        u0[i] = 
    elif 1.2 < xval < 1.5:
        u0[i] = 
    else:
        u0[i] = 
```

Run the following cell to advance the solution using the time-stepping function provided above


```python
u_adv = advance_solution(u0, a, dx, dt, tf)
```

Compute the total variation (TV) of the initial and the final solution. Is this method TVD?

Complete the TV calculation of the final solution.


```python
tv_u0 = np.sum(np.abs(u0[1:] - u0[:-1]))
tv_uf = 

if tv_uf <= tv_u0:
    print('Scheme is TVD')
else:
    print('Scheme is not TVD')
```

Plot the solution of the second-order upwind-biased scheme and compare with initial solution


```python
plt.figure(0)
plt.plot(x, u0, 'k', lw=1, label=r'$u(x,0)$')
plt.plot(x, u_adv, 'o', color='#bd0c00', ms=1.5, label=r'$u(x,t^*)$')
plt.legend()
```

To preserve the monotonicity of the scheme, we need to define limiter functions. Complete the ``van_leer`` and ``superbee`` limiters using the formulations shown in the textbook.


```python
def minmod(r):
    return max([0, min([1, r])])
    
def van_leer(r):
    return 

def superbee(r):
    return 
```

Complete the ``get_slope`` function below to include the limiters following the theory in the textbook, then run the cell. 
**Note:**, you will need to add ``ep`` to any division by ``r`` to avoid infinity.


```python
def get_slope(u0, u1, u2, b):
    ep = 1e-20
    # Compute ratio of slopes
    r = 
    phi = limiter(r)
    # Compute limited slope
    delta = 
    return delta
```

For each of the limiter functions implemented above, generate a plot of the solution to compare results.


```python
fig, ax = plt.subplots(figsize=(8, 2.5), ncols=3, sharex=True, sharey=True)

# Minmod limiter
limiter = minmod
u_adv = advance_solution(u0, a, dx, dt, tf) 
ax[0].plot(x, u0, 'k', lw=1)
ax[0].plot(x, u_adv, 'o', markersize=1.5, color='#bd0c00')
ax[0].set_ylabel('$u$')
ax[0].set_xlabel('$x$')
ax[0].set_title('minmod')

# van Leer limiter
limiter = van_leer
u_adv = advance_solution(u0, a, dx, dt, tf) 
ax[1].plot(x, u0, 'k', lw=1)
ax[1].plot(x, u_adv, 'o', markersize=1.5, color='#bd0c00')
ax[1].set_ylabel('$u$')
ax[1].set_xlabel('$x$')
ax[1].set_title('van Leer')

# Superbee limiter
limiter = superbee
u_adv = advance_solution(u0, a, dx, dt, tf) 
ax[2].plot(x, u0, 'k', lw=1)
ax[2].plot(x, u_adv, 'o', markersize=1.5, color='#bd0c00')
ax[2].set_ylabel('$u$')
ax[2].set_xlabel('$x$')
ax[2].set_title('superbee')

plt.tight_layout()
```

## Activities
- What are the advantages and disadvantages of increasing the order of the scheme?
- Check whether the schemes with limiter functions are now TVD
- Modify the definition of ``residual`` to use a Riemann solver of the form $F(u_L, u_R)$. To achieve this, we have implemented a function named ``riemann_solver``. Complete the code to ompute the ``dr`` slopes at each interface. Templates are provided below.
- Note: 
    - Additional ``if`` statements need to be added to enforce periodic boundary conditions to compute ``ur`` and ``dr``.
    - For linear advection, the Riemann solver is expected to return only ``ul``.


```python
def riemann_solver(ul, ur):
    return ul
    
def residual(u, a, dx):
    res = 0.0*u
    b = 0.0
    for i in range(n):
        # Interface i+1/2
        if i == n - 1:
            # Enforce periodic BC at x=L
            dl = get_slope(u[i - 1], u[i], u[0], b)
            dr = 
        elif i == n - 2:
            dl = 
            dr = 
        else:
            dl = get_slope(u[i - 1], u[i], u[i + 1], b)
            dr = 
            
        # Compute u_i+1/2,L and u_i+1/2,R
        ul = u[i] + 0.5*dl
        ur = 
        
        # Compute f_i+1/2 using the upwind Riemann solver
        fR = riemann_solver(ul, ur)
        
        # Interface i-i/2
        dl = get_slope(u[i - 2], u[i - 1], u[i], b) 
        dr = 
        
        # Compute u_i-1/2,L and u_i-1/2,R
        ul = u[i - 1] + 0.5*dl
        ur = 
        
        # Computr f_i-1/2 using the upwind Rieman solver
        fL = riemann_solver(ul, ur)
        
        res[i] = -(fR - fL)/dx
    return res
```

Rewrite ``riemann_solver`` so that it uses a Roe solver for the Burgers equation. Then, run the following cell to see your implementation


```python
n = 100
L = 1.0
dt = 0.001
tf = 0.5

# Calculate mesh spacing and grid points
dx = L/n 
x = np.arange(0, L, dx)

# Define limiter to use
limiter = minmod

u0 = np.exp(-40 * (x - 1/2)**2)
u_burgers = advance_solution(u0, 1, dx, dt, tf) 

plt.figure()
plt.plot(x, u_burgers)
```



