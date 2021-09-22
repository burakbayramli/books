"""
Author: Rohan
Date: 19/03/16
"""
# Standard includes
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import numpy as np

# Project includes
from CFD_Projects.Twelve_Steps.models.diffusion_model import Diffusion


def plot_mass_diffusivity_co2():
    """
    This function is used to plot a simple example of calculating the mass diffusivity based
    on an empirical relationship with temperature
    """
    temperature = np.linspace(300, 310, 1000)

    diffusivity = Diffusion.mass_diffusivity(temperature)

    plt.figure(figsize=(10, 8))
    plt.plot(temperature, diffusivity)
    plt.xlabel("Temperature (K)")
    plt.ylabel("Diffusivity (mm^2/s)")
    plt.title("Mass Diffusion vs Temperature")
    plt.show()


def plot_1d_diffusion():
    """
    This function is used to plot a simple example of 1D diffusion
    """
    x_max = 2.0
    nx = 41
    dx = x_max / (nx - 1)
    nt = 100
    sigma = 0.2
    nu = 0.3
    dt = sigma * dx ** 2 / nu

    x = np.linspace(0, x_max, nx)
    t = np.linspace(0, (nt - 1) * dt, nt)

    # set up initial conditions
    u_initial = np.ones(x.shape)
    u_initial[nx/4:nx/2] = 2

    u_solution = Diffusion.diffusion_1d(x, t, u_initial, nu=nu)
    # plot final time step
    for i in range(0, nt, 20):
        plt.figure()
        plt.plot(u_solution[:, i])
        plt.title("Diffuse Term vs. x Plot for final time step of simulation at timestep:"
                  + str(i))
        plt.ylabel("Diffuse Term")
        plt.xlabel("x location (m)")
        plt.ylim([0.95, 2.1])
        plt.show()


def plot_diffusion_2d():
    """
    This function is used to plot a simple example of 2D diffusion
    """
    nx = 31
    ny = 31
    nt = 17
    x_max = 2
    y_max = 2
    nu = 0.05
    sigma = 0.25
    dx = float(x_max) / (nx - 1)
    dy = float(y_max) / (ny - 1)
    dt = sigma * dx * dy / nu

    x = np.linspace(0, 2, nx)
    y = np.linspace(0, 2, ny)
    t = np.linspace(0, nt*dt, nt)

    u_initial = np.ones((nx, ny))
    u_initial[nx/4:nx/2, ny/4:ny/2] = 2

    u_sol = Diffusion.diffusion_2d(x, y, t, u_initial, nu=nu)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, u_sol[0, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Initial Diffusion Condition")

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, u_sol[-1, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Final Diffusion Condition")


def plot_burgers_equation_1d():
    """
    This function is used to calculate a simple example of a combined advection and diffusion
    case by applying Burger's equation
    """
    nx = 101
    nt = 100
    dx = 2 * np.pi/(nx - 1)
    nu = 0.07

    x, u, u_analytical = Diffusion.burgers_equation_1d(nx, nt, dx, nu_value=nu)

    plt.figure()
    plt.plot(x, u[:], marker='o', lw=2, label="computational")
    plt.plot(x, u_analytical[:], label="analytical")
    plt.title("Burger's Equation Example")
    plt.ylabel("Diffuse Term")
    plt.xlabel("x location (m)")
    plt.xlim([0, 2*np.pi])
    plt.ylim([0, 10])
    plt.show()


def plot_burgers_equation_2d():
    """
    This function is used to calculate a simple example of combined advection and diffusion in
    2D
    """
    x_max = 2.0
    y_max = 2.0
    nx = 41
    ny = 41
    nt = 120
    dx = x_max/nx
    dy = y_max/ny
    sigma = 0.0009
    nu=0.01
    dt = sigma*dx*dy/nu
    x = np.linspace(0, x_max, nx)
    y = np.linspace(0, y_max, ny)
    t = np.linspace(0, nt * dt, nt)

    u_initial = np.ones((x.shape[0], y.shape[0]))
    u_initial[nx/4:nx/2+1, ny/4:ny/2+1] = 2
    v_initial = np.ones((x.shape[0], y.shape[0]))
    v_initial[nx/4:nx/2+1, ny/4:ny/2+1] = 2

    u_sol, v_sol = Diffusion.burgers_equation_2d(x, y, t, u_initial, v_initial, nu=nu)

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, u_sol[0, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Initial Burgers Condition u")

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, u_sol[-1, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Final Burgers Condition u")

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, v_sol[0, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Initial Burgers Condition v")

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X, Y = np.meshgrid(x, y)
    surf = ax.plot_surface(X, Y, v_sol[-1, :, :], rstride=1, cstride=1,
                           cmap=cm.coolwarm, linewidth=0, antialiased=False)
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)
    ax.set_zlim(1, 2.5)
    fig.savefig("Final Burgers Condition v")


if __name__ == '__main__':
    # plot_mass_diffusivity_co2()
    # plot_1d_diffusion()
    # plot_diffusion_2d()
    # plot_burgers_equation_1d()
    plot_burgers_equation_2d()
