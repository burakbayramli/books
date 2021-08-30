# finite size correction factors for the flat punch problem

# import the python modules that we need
from sympy import symbols, diff, lambdify, simplify
import numpy as np
import matplotlib.pyplot as plt

# make and label axes
fig, ax = plt.subplots(1,1, figsize=(3,3))
ax.set_xlabel('a/h')
ax.set_ylabel('correction factor')
fig.tight_layout()

# create the symbolic functions that we need
ah = symbols('ah')
fC=(1+1.33*(ah)+1.33*(ah) ** 3)**(-1)
fGp = -(ah ** 2)*diff(fC/ah, ah)
fGd = fGp/fC**2


# convert symbolic functions to callable numpy functions and plot them
ah_vals=np.linspace(0.01, 5, 100)
sym_functions = [fC, fGp, fGd]
markers = ['-','--', ':']
labels = [r'$f_C$', r'$f_{\mathcal{G}p}$', r'$f_{\mathcal{G}\delta}$']
for i in np.arange(len(sym_functions)):
    func = lambdify(ah, sym_functions[i], 'numpy')
    ax.semilogy(ah_vals, func(ah_vals), markers[i], label=labels[i])
    
# add legend, clean up some details of the figure and save the image file
ax.legend()
ax.set_xlim(left=0)
ax.set_xticks(np.arange(6))
fig.savefig('../figures/flat_punch_finite_size_corrections.svg')

#%%  plot of normalized G vs. displacement (from Rebecca Webber's paper)
plt.close('all')
ah_vals=np.linspace(0.01, 2, 100)
Gd_norm = (2/(3*np.pi))*(1/ah)*fGd
Gd_norm2 = (2/(3*np.pi))*fGd
Gd_func = lambdify(ah, Gd_norm, 'numpy')
Gd_func2 = lambdify(ah, Gd_norm2, 'numpy')

fig2, ax2 = plt.subplots(1,2, figsize=(6,3))
ax2[0].plot(ah_vals, Gd_func(ah_vals),'-')
ax2[0].set_ylim([1,5])
ax2[0].set_xlabel('a/h')
ax2[0].set_ylabel(r'$\mathcal{G}h/E_r\delta_t^2$')

ax2[1].plot(ah_vals, Gd_func2(ah_vals),'-')
ax2[1].set_xlabel('a/h')
ax2[1].set_ylabel(r'$\mathcal{G}a/E_r\delta_t^2$')

fig2.tight_layout()
fig2.savefig('../figures/confinement_normGd.svg')



