"""
Author: Rohan
Date: 20/03/16
"""
# Standard imports
import numpy as np
from matplotlib import pyplot as plt

# Project imports
from CFD_Projects.Twelve_Steps.models.navier_stokes_model import NavierStokes
from CFD_Projects.Twelve_Steps.plots.plot_functions import Plot


def plot_1d_convection():
    """
    Function used to plot an example of 1d linear and non-linear convection
    """
    x_max = 2.0
    nx = 1000
    t_max = 0.625
    nt = 1000
    x = np.linspace(0, x_max, nx)
    t = np.linspace(0, t_max, nt)

    u_initial = np.ones(x.shape)
    u_initial[nx/4:nx/2] = 2

    u_solution_linear = NavierStokes.convection_1d(x, t, u_initial, linear=True)
    u_solution_non_linear = NavierStokes.convection_1d(x, t, u_initial, linear=False)

    # plot final time step
    plt.figure()
    plt.plot(u_solution_linear[:, -1], label="Linear")
    plt.plot(u_solution_non_linear[:, -1], label="Non-linear")
    plt.ylim([0.95, 2.1])
    plt.legend()
    plt.title("Velocity vs. x Plot for final time step of simulation")
    plt.ylabel("velocity (m/s)")
    plt.xlabel("x location (m)")
    plt.show()


def plot_2d_convection():
    """
    Function used to plot 2D convection for linear and non-linear cases in 2D
    """
    x_max = 2.0
    y_max = 2.0
    nx = 81
    ny = 81
    t_max = 0.3125
    nt = 100
    x = np.linspace(0, x_max, nx)
    y = np.linspace(0, y_max, ny)
    t = np.linspace(0, t_max, nt)

    u_initial = np.ones((x.shape[0], y.shape[0]))
    u_initial[nx/4:nx/2+1, ny/4:ny/2+1] = 2
    v_initial = np.ones((x.shape[0], y.shape[0]))
    v_initial[nx/4:nx/2+1, ny/4:ny/2+1] = 2

    u_solution_linear, v_solution_linear = NavierStokes.convection_2d(x, y, t, u_initial, v_initial, linear=True)

    Plot.plot2d(x, y, v_solution_linear[0, :, :], '2d_convection_initial_time.png')
    Plot.plot2d(x, y, v_solution_linear[-1, :, :], '2d_convection_final_time.png')

    u_solution, v_solution = NavierStokes.convection_2d(x, y, t, u_initial, v_initial, linear=False)

    Plot.plot2d(x, y, u_solution[-1, :, :], '2d_convection_non_linear_u.png')
    Plot.plot2d(x, y, v_solution[-1, :, :], '2d_convection_non_linear_v.png')


def plot_2d_laplace():
    """
    Function used to plot a simple solution to the Laplace equation in 2D
    """
    nx = 31
    ny = 31
    c = 1
    dx = 2/(nx - 1)
    dy = 2/(ny - 1)

    u = np.zeros((nx, ny))

    x = np.linspace(0, 2, nx)
    y = np.linspace(0, 1, ny)

    u[:, -1] = y

    u_sol = NavierStokes.laplace_2d(x, y, u)

    Plot.plot2d(x, y, u, "laplace_2d_initial")
    Plot.plot2d(x, y, u_sol, "laplace_2d_final")


def plot_2d_poisson():
    """
    Function used to plot a simple solution to the Poisson equation in 2D
    """
    nx = 50
    ny = 50
    nit = 100
    xmin = 0
    xmax = 2
    ymin = 0
    ymax = 1

    x = np.linspace(xmin, xmax, nx)
    y = np.linspace(ymin, ymax, ny)
    u = np.zeros((nx, ny))
    b = np.zeros((nx, ny))

    b[nx/4, ny/4] = 100
    b[3 * nx/4, 3 * ny/4] = -100

    u_sol = NavierStokes.poisson_2d(x, y, u, b, nit)

    Plot.plot2d(x, y, u, "Poisson_initial")
    Plot.plot2d(x, y, u_sol, "Poisson_final")


def plot_cavity_flow():
    """
    Function used to plot a simple solution to the cavity flow simulation in the Navier Stokes
    static simulation class
    """
    nx = 41
    ny = 41
    nt = 1000
    nit = 50
    c = 1
    x = np.linspace(0, 2, nx)
    y = np.linspace(0, 2, ny)

    # Physical Parameters
    rho = 100
    nu = 0.1
    dt = 0.001

    time_steps = np.linspace(0, dt * nt, nt)

    u = np.zeros((nx, ny))
    v = np.zeros((nx, ny))
    p = np.zeros((nx, ny))

    u, v, p = NavierStokes.cavity_flow(x, y, time_steps, u, v, p, rho, nu, nit)

    u = np.transpose(u)
    v = np.transpose(v)
    p = np.transpose(p)
    Plot.plot2d_vector_contour(x, y, p, u, v, 'Cavity_Flow')


def plot_channel_flow():
    """
    A function to plot a simple solution to channel flow
    """
    nx = 41
    ny = 41
    nit = 50
    x = np.linspace(0, 2, nx)
    y = np.linspace(0, 2, ny)
    error = 1e-6

    rho = 1
    nu = 0.1
    F = 1
    dt = 0.01

    u = np.zeros((nx, ny))
    v = np.zeros((nx, ny))
    p = np.zeros((nx, ny))

    u, v, p = NavierStokes.channel_flow(x, y, error, dt, u, v, p, rho, nu, nit, F)

    u = np.transpose(u)
    v = np.transpose(v)

    Plot.plot2d_vector(x, y, u, v, "Channel_Flow")

if __name__ == '__main__':
    # plot_1d_convection()
    # plot_2d_convection()
    # plot_2d_laplace()
    # plot_2d_poisson()
    plot_cavity_flow()
    # plot_channel_flow()
