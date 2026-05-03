import numpy as np
import matplotlib.pyplot as plt

# --- Constants ---
boltz = 1.3806e-23
mass  = 6.63e-26
diam  = 3.66e-10
T     = 273.0
density = 1.78
L     = 1e-6

#npart = int(input('Enter number of simulation particles: '))
npart = 10
eff_num = density / mass * L**3 / npart
print(f'Each particle represents {eff_num:.4g} atoms')

# --- Initial positions and velocities ---
rng = np.random.default_rng(0)
x = L * rng.random(npart)
v_init = np.sqrt(3 * boltz * T / mass)
v = np.zeros((npart, 3))
v[:, 0] = v_init * (1 - 2 * np.floor(2 * rng.random(npart)))

# --- Initial speed histogram ---
vmag = np.linalg.norm(v, axis=1)
vbin = np.arange(50, 1100, 100)
plt.figure(1)
plt.hist(vmag, bins=vbin)
plt.title('Initial speed distribution')
plt.xlabel('Speed (m/s)')
plt.ylabel('Number')
plt.tight_layout()
plt.show(block=False)

# --- Collision parameters ---
ncell   = 15
tau     = 0.2 * (L / ncell) / v_init
vrmax   = 3 * v_init * np.ones(ncell)
selxtra = np.zeros(ncell)
coeff   = 0.5 * eff_num * np.pi * diam**2 * tau / (L**3 / ncell)
coltot  = 0


def sorter(x, L, ncell, npart):
    """Sort particles into cells, return cell counts, start indices, and sorted refs."""
    cell_n = np.zeros(ncell, dtype=int)
    jc = np.floor(x / L * ncell).astype(int)
    jc = np.clip(jc, 0, ncell - 1)
    for i in range(npart):
        cell_n[jc[i]] += 1
    index = np.zeros(ncell, dtype=int)
    index[0] = 0
    for i in range(1, ncell):
        index[i] = index[i-1] + cell_n[i-1]
    # Build Xref: sorted particle indices by cell
    Xref = np.zeros(npart, dtype=int)
    count = index.copy()
    for i in range(npart):
        c = jc[i]
        Xref[count[c]] = i
        count[c] += 1
    return cell_n, index, Xref


def colider(v, vrmax, tau, selxtra, coeff, cell_n, index, Xref, ncell):
    """Perform DSMC collisions cell by cell."""
    col = 0
    for jcell in range(ncell):
        np_cell = cell_n[jcell]
        if np_cell < 2:
            continue
        # Number of candidate pairs to select
        select = coeff * np_cell * (np_cell - 1) * vrmax[jcell] + selxtra[jcell]
        nsel = int(select)
        selxtra[jcell] = select - nsel

        crmax = vrmax[jcell]
        start = index[jcell]

        for _ in range(nsel):
            # Pick two distinct random particles in this cell
            k  = int(np_cell * np.random.random())
            kk = int((np_cell - 1) * np.random.random())
            if kk >= k:
                kk += 1
            ip1 = Xref[start + k]
            ip2 = Xref[start + kk]

            # Relative speed
            cr = np.linalg.norm(v[ip1] - v[ip2])
            if cr > crmax:
                crmax = cr

            # Accept/reject collision
            if cr / vrmax[jcell] > np.random.random():
                col += 1
                # Update vrmax
                vrmax[jcell] = crmax
                # Center of mass velocity
                vcm = 0.5 * (v[ip1] + v[ip2])
                # Random unit vector
                cos_th = 1.0 - 2.0 * np.random.random()
                sin_th = np.sqrt(1.0 - cos_th**2)
                phi    = 2.0 * np.pi * np.random.random()
                vrel   = np.array([
                    cr * cos_th,
                    cr * sin_th * np.cos(phi),
                    cr * sin_th * np.sin(phi)
                ])
                v[ip1] = vcm + 0.5 * vrel
                v[ip2] = vcm - 0.5 * vrel

    return v, vrmax, selxtra, col


# --- Main time-stepping loop ---
#nstep = int(input('Enter total number of time steps: '))
nstep = 20
for istep in range(1, nstep + 1):
    # Move particles
    x += v[:, 0] * tau
    x  = np.mod(x + L, L)

    # Sort into cells
    cell_n, index, Xref = sorter(x, L, ncell, npart)

    # Collisions
    v, vrmax, selxtra, coltot_new = colider(
        v, vrmax, tau, selxtra, coeff, cell_n, index, Xref, ncell)
    coltot += coltot_new

    # Periodic display
    if istep % 10 == 0:
        vmag = np.linalg.norm(v, axis=1)
        plt.figure(2)
        plt.clf()
        plt.hist(vmag, bins=vbin)
        plt.title(f'Step {istep}/{nstep}; {coltot} collisions')
        plt.xlabel('Speed (m/s)')
        plt.ylabel('Number')
        plt.tight_layout()
        plt.pause(0.01)

# --- Final distribution ---
vmag = np.linalg.norm(v, axis=1)
plt.figure(2)
plt.clf()
plt.hist(vmag, bins=vbin)
plt.title(f'Final distribution, Time = {nstep*tau:.4g} s')
plt.xlabel('Speed (m/s)')
plt.ylabel('Number')
plt.tight_layout()
plt.savefig('dsmceq1.jpg')
