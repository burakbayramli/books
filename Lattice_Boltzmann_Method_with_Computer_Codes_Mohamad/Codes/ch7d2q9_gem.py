import numpy as np
import matplotlib.pyplot as plt

# Grid and parameters
m, n = 101, 101
xl, yl = 1.0, 1.0
w0 = 4.0 / 9.0
c2 = 1.0 / 3.0
dx, dy = 1.0, 1.0

# Initialize arrays
f0 = np.zeros((m, n))
f = np.zeros((m, n, 8))
feq = np.zeros((m, n, 8))
f0eq = np.zeros((m, n))
rho = np.zeros((m, n))

x = np.zeros(m)
y = np.zeros(n)
# Note: fluxq, flux, and Tm initialized but matches original script allocation sizes
fluxq = np.zeros(m)
flux = np.zeros(m)
Tm = np.zeros(n)  # Adjusted to 'n' to match the final plotting loop length
Z = np.zeros((n, m))

# Grid generation
for i in range(m - 1):
    x[i + 1] = x[i] + dx

for j in range(n - 1):
    y[j + 1] = y[j] + dy

# Weights
w = np.zeros(8)
for k in range(4):
    w[k] = 1.0 / 9.0
for k in range(4, 8):
    w[k] = 1.0 / 36.0

alpha = 1.00
u, v = 0.1, 0.4
omega = 1.0 / (3.0 * alpha + 0.5)
twall = 1.0
nstep = 400

for kk in range(nstep):

    # --- Collision Step ---
    # Vectorized equilibrium calculations for efficiency
    f0eq = w0 * rho
    f0 = (1.0 - omega) * f0 + omega * f0eq

    feq[:, :, 0] = w[0] * rho * (1.0 + 3.0 * u)
    feq[:, :, 1] = w[1] * rho * (1.0 + 3.0 * v)
    feq[:, :, 2] = w[2] * rho * (1.0 - 3.0 * u)
    feq[:, :, 3] = w[3] * rho * (1.0 - 3.0 * v)
    feq[:, :, 4] = w[4] * rho * (1.0 + 3.0 * (u + v))
    feq[:, :, 5] = w[5] * rho * (1.0 + 3.0 * (v - u))
    feq[:, :, 6] = w[6] * rho * (1.0 - 3.0 * (u + v))
    feq[:, :, 7] = w[7] * rho * (1.0 + 3.0 * (u - v))

    for k in range(8):
        f[:, :, k] = (1.0 - omega) * f[:, :, k] + omega * feq[:, :, k]

    # --- Streaming Step (using np.roll) ---
    f[:, :, 0] = np.roll(f[:, :, 0], shift=(1, 0), axis=(0, 1))
    f[:, :, 1] = np.roll(f[:, :, 1], shift=(0, 1), axis=(0, 1))
    f[:, :, 2] = np.roll(f[:, :, 2], shift=(-1, 0), axis=(0, 1))
    f[:, :, 3] = np.roll(f[:, :, 3], shift=(0, -1), axis=(0, 1))
    f[:, :, 4] = np.roll(f[:, :, 4], shift=(1, 1), axis=(0, 1))
    f[:, :, 5] = np.roll(f[:, :, 5], shift=(-1, 1), axis=(0, 1))
    f[:, :, 6] = np.roll(f[:, :, 6], shift=(-1, -1), axis=(0, 1))
    f[:, :, 7] = np.roll(f[:, :, 7], shift=(1, -1), axis=(0, 1))

    # --- Boundary Conditions ---
    # Left boundary (i = 0)
    f[0, :, 0] = w[0] * twall + w[2] * twall - f[0, :, 2]
    f[0, :, 4] = w[4] * twall + w[6] * twall - f[0, :, 6]
    f[0, :, 7] = w[7] * twall + w[5] * twall - f[0, :, 5]

    # Bottom boundary (j = 0), T = 0.0
    f[:, 0, 1] = -f[:, 0, 3]
    f[:, 0, 4] = -f[:, 0, 6]
    f[:, 0, 5] = -f[:, 0, 7]

    # Top boundary (j = m-1 or n-1 based on original loop logic), T = 0.0
    # Note: The original MATLAB script used `for i=1:n; f(i,m,7)=...` 
    # Python conversion keeps indices structurally intact via -1 adjustments.
    f[:, m - 1, 6] = -f[:, m - 1, 4]
    f[:, m - 1, 3] = -f[:, m - 1, 1]
    f[:, m - 1, 7] = -f[:, m - 1, 5]

    # Right hand boundary (i = n-1)
    f[n - 1, :, 2] = -f[n - 1, :, 0]
    f[n - 1, :, 6] = -f[n - 1, :, 4]
    f[n - 1, :, 5] = -f[n - 1, :, 7]

    # --- Update Density (rho) ---
    sumk = np.sum(f, axis=2)
    rho = f0 + sumk

# Post-processing map assignments
for j in range(n):
    for i in range(m):
        Z[j, i] = rho[i, j]

# Extract middle slice (using floor division for Python index safety)
mid_index = int((n - 1) / 2)
for i in range(n):
    Tm[i] = rho[i, mid_index]

# --- Plotting ---
# Figure 1: Line plot
plt.figure(1)
plt.plot(x, Tm, linewidth=2)
plt.xlabel("X")
plt.ylabel("T")
plt.grid(True)
plt.savefig('/tmp/out1.jpg')

# Figure 2: Contour plot
plt.figure(2)
contour_plot = plt.contour(Z, linewidths=2)
plt.clabel(contour_plot, inline=True, fontsize=8, fmt="%1.2f")
plt.title("Contour Profile")

plt.savefig('/tmp/out2.jpg')
