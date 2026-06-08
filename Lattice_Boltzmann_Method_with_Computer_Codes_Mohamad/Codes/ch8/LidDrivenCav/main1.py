"""
LBM - 2D D2Q9, Lid-driven Cavity
Translation of Mohamad's MATLAB code, numpy-vectorised where possible.
Done by Claude Sonnet 4.6
"""

import numpy as np
import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------
nx, ny = 101, 101

f   = np.zeros((nx, ny, 9))
u   = np.zeros((nx, ny))
v   = np.zeros((nx, ny))
rho = np.ones((nx, ny))

w  = np.array([1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36, 4/9])
cx = np.array([1,  0, -1,  0,  1, -1, -1,  1,  0])
cy = np.array([0,  1,  0, -1,  1,  1, -1, -1,  0])

xl, yl = 1.0, 1.0
dx = xl / (nx - 1)
dy = yl / (ny - 1)
x  = np.linspace(0, xl, nx)
y  = np.linspace(0, yl, ny)

uo    = 0.10
alpha = 0.1
Re    = uo * (ny - 1) / alpha
omega = 1.0 / (3.0 * alpha + 0.5)

print(f"Re = {Re}")

tol   = 1e-4
error = 10.0
erso  = 0.0
count = 0

# Lid velocity
u[:, -1] = uo

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

def collision(f, u, v, rho):
    t1 = u**2 + v**2                                    # (nx, ny)
    # t2[k] = u*cx[k] + v*cy[k]  for each direction k
    t2 = (u[:, :, np.newaxis] * cx
        + v[:, :, np.newaxis] * cy)                     # (nx, ny, 9)
    feq = (rho[:, :, np.newaxis] * w
           * (1.0 + 3.0*t2 + 4.5*t2**2
              - 1.5*t1[:, :, np.newaxis]))
    f = (1.0 - omega) * f + omega * feq
    return f


def stream(f):
    # shifts match MATLAB circshift([+1,0]) etc.
    # MATLAB: circshift(A, [r,c]) shifts rows by r, cols by c
    # np.roll(a, shift, axis): positive shift → elements move toward higher index
    shifts = [(1,0),(0,1),(-1,0),(0,-1),(1,1),(-1,1),(-1,-1),(1,-1)]
    for k, (sr, sc) in enumerate(shifts):
        f[:, :, k] = np.roll(np.roll(f[:, :, k], sr, axis=0), sc, axis=1)
    return f


def boundary(f, uo):
    # --- Left boundary: bounce-back ---
    f[0, :, 0] = f[0, :, 2]   # k=1 -> k=3
    f[0, :, 4] = f[0, :, 6]   # k=5 -> k=7
    f[0, :, 7] = f[0, :, 5]   # k=8 -> k=6

    # --- Right boundary: bounce-back ---
    f[-1, :, 2] = f[-1, :, 0]  # k=3 -> k=1
    f[-1, :, 6] = f[-1, :, 4]  # k=7 -> k=5
    f[-1, :, 5] = f[-1, :, 7]  # k=6 -> k=8

    # --- Bottom boundary: bounce-back ---
    f[:, 0, 1] = f[:, 0, 3]    # k=2 -> k=4
    f[:, 0, 4] = f[:, 0, 6]    # k=5 -> k=7
    f[:, 0, 5] = f[:, 0, 7]    # k=6 -> k=8

    # --- Top boundary: moving lid (Zou/He) ---
    # vectorised over interior x nodes (index 1..nx-2)
    i = slice(1, nx - 1)
    rhon = (f[i, -1, 8] + f[i, -1, 0] + f[i, -1, 2]
            + 2.0 * (f[i, -1, 1] + f[i, -1, 5] + f[i, -1, 4]))
    f[i, -1, 3] = f[i, -1, 1]                       # k=4 = k=2
    f[i, -1, 7] = f[i, -1, 5] + rhon * uo / 6.0    # k=8 = k=6 + ...
    f[i, -1, 6] = f[i, -1, 4] - rhon * uo / 6.0    # k=7 = k=5 - ...

    return f


def ruv(f):
    rho = f.sum(axis=2)

    # Correct top-row density (non-equilibrium extrapolation for moving lid)
    rho[:, -1] = (f[:, -1, 8] + f[:, -1, 0] + f[:, -1, 2]
                  + 2.0 * (f[:, -1, 1] + f[:, -1, 5] + f[:, -1, 4]))

    u = (f[:, :, 0] + f[:, :, 4] + f[:, :, 7]
       - f[:, :, 2] - f[:, :, 5] - f[:, :, 6]) / rho

    v = (f[:, :, 1] + f[:, :, 4] + f[:, :, 5]
       - f[:, :, 3] - f[:, :, 6] - f[:, :, 7]) / rho

    return rho, u, v


# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------
while error > tol:
    print(f"error = {error:.6e}")

    f = collision(f, u, v, rho)
    f = stream(f)
    f = boundary(f, uo)
    rho, u, v = ruv(f)

    count += 1
    ers   = np.sum(u**2 + v**2)
    error = abs(ers - erso)
    erso  = ers

print(f"Converged in {count} iterations.")

# ---------------------------------------------------------------------------
# Post-processing (mirrors result.m)
# ---------------------------------------------------------------------------
mid_x = (nx - 1) // 2
mid_y = (ny - 1) // 2

um = u[mid_x, :] / uo   # centreline u-velocity vs y
vm = v[:, mid_y] / uo   # centreline v-velocity vs x

fig, ax = plt.subplots()
X, Y = np.meshgrid(x, y)
speed = np.sqrt(u**2 + v**2)
strm = ax.streamplot(X, Y, u.T, v.T, color=speed.T, cmap='viridis', linewidth=1.5)
fig.colorbar(strm.lines, label='Velocity Magnitude')
ax.set_title("Lid-Driven Cavity Flow")
ax.set_xlabel("X"); ax.set_ylabel("Y")
fig.savefig("/tmp/out0.jpg", dpi=150)

fig, ax = plt.subplots()
ax.plot(um, y, linewidth=1.5)
ax.set_xlabel("U/Uo"); ax.set_ylabel("Y")
ax.set_title("U-velocity profile at cavity centreline")
fig.savefig('/tmp/out1.jpg')

fig, ax = plt.subplots()
ax.plot(x, vm, linewidth=1.5)
ax.set_xlabel("X"); ax.set_ylabel("V/Uo")
ax.set_title("V-velocity profile at cavity centreline")
fig.savefig('/tmp/out2.jpg')

s = 4  # plot every 4th point
fig, ax = plt.subplots()
ax.quiver(x[::s], y[::s], (v[::s, ::s] / uo).T, (u[::s, ::s] / uo).T)
ax.set_title("Velocity quiver")

fig.savefig('/tmp/out3.jpg')

# Stream function (trapezoidal integration upward in j)
str_ = np.zeros((nx, ny))
for j in range(1, ny):
    str_[:, j] = (str_[:, j-1]
                  + 0.25 * (rho[:, j] + rho[:, j-1])
                          * (u[:, j] + u[:, j-1]))

fig, ax = plt.subplots()
ax.contour(x, y, str_.T, levels=20)
ax.set_title("Stream function contours")

fig.savefig('/tmp/out4.jpg')
