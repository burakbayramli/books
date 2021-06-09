from fvm_classes import *
import numpy as np
from scipy.sparse import linalg
from clint.textui import progress

# matplotlib for movie export here
#   http://matplotlib.org/examples/animation/moviewriter.html

comment = "In all these example numerical instabilities are exist, this is to illustrate the\n\
difference between second order central difference, first order upwind and exponentially\n\
fitted discretisation schemes. The Peclet number is around 20 which means that central\n\
discretisation are numerically unstable (they are only stable with Peclet number below\n\
2). Upwind scheme do not have any restriction on Peclet number for stability, however\n\
they introduce numerical (artificial) diffusion to the solution. Exponentially fitting\n\
guarantees stability (with regard to Peclet number) because it is a hybrid\n\
discretisation scheme which combines an weighted averages the central and upwind\n\
approaches. The the Peclet number is large the discretisation is weighted in favor of\n\
the upwind scheme, and when the Peclet number is low a central scheme is favored."


def hundsdorfer_examples(faces, export_filename="movie.mp4"):
    
    a = 1        # Advection velocity
    d = 1e-3     # Diffusion coefficient
    k = 0.01     # Time step 
    
    log = "Comparison of central, upwind and exponential fitting schemes."
    print log
    central = Model(faces, a, d, k, discretisation="central")
    central.set_boundary_conditions(left_value=1., right_value=0.)
    A1 = central.A_matrix()
    M1 = central.M_matrix()
    b1 = central.b_vector()
    
    upwind = Model(faces, a, d, k, discretisation="upwind")
    upwind.set_boundary_conditions(left_value=1., right_value=0.)
    A2 = upwind.A_matrix()
    M2 = upwind.M_matrix()
    b2 = upwind.b_vector()
    
    exponential = Model(faces, a, d, k, discretisation="exponential")
    exponential.set_boundary_conditions(left_value=1., right_value=0.)
    A3 = exponential.A_matrix()
    M3 = exponential.M_matrix()
    b3 = exponential.b_vector()
    
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.animation as manimation
    print manimation.writers.__dict__
    FFMpegWriter = manimation.writers['ffmpeg']
    metadata = dict(title=log, artist='https://github.com/danieljfarrell/FVM', comment=comment)
    writer = FFMpegWriter(fps=24, metadata=metadata)
    
    fig = plt.figure()
    l0, = plt.plot([],[], 'k-', lw=1)
    l_central, = plt.plot([],[], 'r-o', label="central", markersize=6, alpha=0.5)
    l_upwind, = plt.plot([],[], 'g-o', label="upwind", markersize=6, alpha=0.5)
    l_exp, = plt.plot([],[], 'b-o', label="exponential", markersize=6, alpha=0.5)
    plt.xlim(-0.1, 1.1)
    plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.05), fancybox=True, shadow=True, ncol=3)
    plt.ylim(-0.2,1.2)
    
    # # Analytical solution for Dirichlet boundary conditions
    mesh = central.mesh
    analytical_x = np.linspace(0,1,2000)
    import sympy
    a_s, h_s, d_s= sympy.var("a h d")
    f = (sympy.exp(a_s/d_s) - sympy.exp(a_s*h_s/d_s))/(sympy.exp(a_s/d_s)-1)
    analytical_solution = [f.subs({a_s:a, d_s:d, h_s:x}) for x in analytical_x ]
    analytical_solution = np.array(analytical_solution)
    
    w_init = np.sin(np.pi*mesh.cells)**100
    w1 = w_init
    w2 = w_init
    w3 = w_init
    
    #exit(1)
    with writer.saving(fig, export_filename, 300):
    
        iters = 251
        for i in progress.bar(range(iters)):
            w1 = linalg.spsolve(A1.tocsc(), M1 * w1 + b1)
            w2 = linalg.spsolve(A2.tocsc(), M2 * w2 + b2)
            w3 = linalg.spsolve(A3.tocsc(), M3 * w3 + b3)
            
            if  i == 0:
                l_central.set_data(mesh.cells, w_init)
                l_upwind.set_data(mesh.cells, w_init)
                l_exp.set_data(mesh.cells, w_init)
                l0.set_data(analytical_x, analytical_solution)
                writer.grab_frame()
            
            if i %  1 == 0 or i == 0:
                l_central.set_data(mesh.cells, w1)
                l_upwind.set_data(mesh.cells, w2)
                l_exp.set_data(mesh.cells, w3)
                l0.set_data(analytical_x, analytical_solution)
                writer.grab_frame()

if __name__ == '__main__':
    
    hundsdorfer_examples(np.linspace(0, 1, 50), export_filename="uniform_grid.mp4")
    faces = np.concatenate((np.array([0]), np.sort(np.random.uniform(0, 1, 48)), np.array([1])))
    hundsdorfer_examples(faces, export_filename="random_grid.mp4")
    def geo_series(n, r):
        total = 0
        series = []
        for i in range(n):
            
            if i == 0:
                total = 1
            else:
                total = total - total*r
            series.append(total)
            
        series = np.array(series)
        norm = series / (np.max(series) - np.min(series))
        series = norm - np.min(norm)
        return np.abs(series - 1)
        
    faces = geo_series(50, 0.15)
    hundsdorfer_examples(faces, export_filename="geometric_grid.mp4")
    