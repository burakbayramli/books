# Converted into Python by Gemini 3 Fast
import numpy as np
import matplotlib.pyplot as plt
import os

# --- Constants & Setup (Volkov Template 08) ---
BOLTZMANN = 1.380662e-23
AVOGADRO = 6.022045e+23
MOLAR_MASS = 0.040 
MOLECULE_MASS = MOLAR_MASS / AVOGADRO

# Domain Boundaries
X1, X2, Y1, Y2 = -1.0, 1.0, -1.0, 1.0
NX, NY = 100, 100
DX, DY = (X2 - X1) / NX, (Y2 - Y1) / NY
DZ = 0.1
DV = DX * DY * DZ

# Flow Parameters
DT = 1.0e-06
T_FREE = 200.0
P_FREE = 0.1
N_FREE = P_FREE / (BOLTZMANN * T_FREE)
MA_FREE = 4.0
U_FREE_MAG = MA_FREE * np.sqrt((5.0/3.0) * BOLTZMANN * T_FREE / MOLECULE_MASS)
ATTACK_ANGLE = np.radians(30.0)

UX_FREE = U_FREE_MAG * np.cos(ATTACK_ANGLE)
UY_FREE = -U_FREE_MAG * np.sin(ATTACK_ANGLE)

# VHS Model Constants
D_REF = 4.17e-10
SIGMA_REF = np.pi * D_REF**2
VISC_INDEX = 0.81
OMEGA = 2.0 * VISC_INDEX - 1.0
CR_REF = 500.0 
WEIGHT = (DV * N_FREE) / 10.0  # Weight based on average 10 particles per cell

# Wing Geometry
WING_X = -0.25
WING_Y = 0.0
WING_LENGTH = 0.5
TW = 300.0

# Global State
pos = np.empty((0, 2))
vel = np.empty((0, 3))

def generate_particles(x_range, y_range, n_gas, move_flag=False):
    global pos, vel
    vol = (x_range[1] - x_range[0]) * (y_range[1] - y_range[0]) * DZ
    n_avg = n_gas * vol / WEIGHT
    n_new = np.random.poisson(n_avg)
    
    new_pos = np.random.uniform([x_range[0], y_range[0]], [x_range[1], y_range[1]], (n_new, 2))
    u_gas = [UX_FREE, UY_FREE, 0.0]
    new_vel = np.random.normal(u_gas, np.sqrt(BOLTZMANN * T_FREE / MOLECULE_MASS), (n_new, 3))
    
    if move_flag:
        new_pos += new_vel[:, :2] * DT
        
    pos = np.vstack([pos, new_pos])
    vel = np.vstack([vel, new_vel])

def collide_particles():
    global vel
    # Indexing: Assign particles to cells
    ix = ((pos[:, 0] - X1) / DX).astype(int)
    iy = ((pos[:, 1] - Y1) / DY).astype(int)
    ix = np.clip(ix, 0, NX-1)
    iy = np.clip(iy, 0, NY-1)
    cell_ids = ix * NY + iy
    
    # Sort for efficient grouping
    sort_idx = np.argsort(cell_ids)
    sorted_cells = cell_ids[sort_idx]
    unique_cells, start_indices, counts = np.unique(sorted_cells, return_index=True, return_counts=True)
    
    # Estimate max collision cross-section relative velocity[cite: 1]
    sc_max = 9.0 * SIGMA_REF * np.sqrt(BOLTZMANN * T_FREE / MOLECULE_MASS)

    for i in range(len(unique_cells)):
        if counts[i] < 2: 
            continue
        
        cell_pcls = sort_idx[start_indices[i] : start_indices[i] + counts[i]]
        
        # NTC: Number of pairs to select[cite: 1]
        n_pairs_avg = 0.5 * counts[i] * (counts[i] - 1) * WEIGHT * sc_max * DT / DV
        n_pairs = int(n_pairs_avg) + (1 if np.random.rand() < (n_pairs_avg - int(n_pairs_avg)) else 0)
        
        for _ in range(n_pairs):
            p1, p2 = np.random.choice(cell_pcls, 2, replace=False)
            v_rel = vel[p1] - vel[p2]
            cr = np.linalg.norm(v_rel)
            sigma = SIGMA_REF * (CR_REF / cr)**OMEGA if cr > 1e-6 else 0
            
            # Acceptance-Rejection for "Real" collision[cite: 1]
            if np.random.rand() < (sigma * cr / sc_max):
                v_cm = 0.5 * (vel[p1] + vel[p2])
                phi = 2 * np.pi * np.random.rand()
                cos_theta = 2 * np.random.rand() - 1
                sin_theta = np.sqrt(1 - cos_theta**2)
                
                n_vec = np.array([sin_theta * np.cos(phi), sin_theta * np.sin(phi), cos_theta])
                vel[p1] = v_cm + 0.5 * cr * n_vec
                vel[p2] = v_cm - 0.5 * cr * n_vec

# --- Main Simulation Loop ---
if not os.path.exists('frames_2d'): 
    os.makedirs('frames_2d')

# Set Initial Conditions[cite: 1]
generate_particles([X1, X2], [Y1, Y2], N_FREE)

print("Starting Simulation with NTC Collisions...")

for step in range(1, 1001):
    # 1. Move Particles[cite: 1]
    pos_old = pos.copy()
    pos += vel[:, :2] * DT
    
    # 2. Wing Interaction (Diffuse Scattering)[cite: 1]
    crossed = (pos_old[:, 1] - WING_Y) * (pos[:, 1] - WING_Y) < 0
    in_x_range = (pos[:, 0] > WING_X) & (pos[:, 0] < WING_X + WING_LENGTH)
    hit_wing = crossed & in_x_range
    
    if np.any(hit_wing):
        rtw = BOLTZMANN * TW / MOLECULE_MASS
        n_hits = np.sum(hit_wing)
        vel[hit_wing, 0] = np.random.normal(0, np.sqrt(rtw), n_hits)
        vel[hit_wing, 2] = np.random.normal(0, np.sqrt(rtw), n_hits)
        side = np.sign(pos_old[hit_wing, 1] - WING_Y)
        vel[hit_wing, 1] = side * np.random.rayleigh(np.sqrt(rtw), n_hits)

    # 3. Boundary Conditions (Inflow/Outflow)[cite: 1]
    dl = 0.1
    generate_particles([X1 - dl, X1], [Y1 - dl, Y2 + dl], N_FREE, True)
    generate_particles([X2, X2 + dl], [Y1 - dl, Y2 + dl], N_FREE, True)
    
    inside = (pos[:, 0] > X1) & (pos[:, 0] < X2) & (pos[:, 1] > Y1) & (pos[:, 1] < Y2)
    pos, vel = pos[inside], vel[inside]

    # 4. Collide Particles (The "Traffic Jam" logic)[cite: 1]
    collide_particles()

    # 5. Visualization (Every 50 steps)
    if step % 50 == 0:
        plt.figure(figsize=(10, 5))
        # Fixed scale vmin/vmax to avoid flickering
        plt.hist2d(pos[:, 0], pos[:, 1], bins=[NX, NY], 
                   range=[[X1, X2], [Y1, Y2]], 
                   cmap='plasma', vmin=0, vmax=50)
        
        plt.plot([WING_X, WING_X + WING_LENGTH], [WING_Y, WING_Y], 'w-', lw=2)
        plt.colorbar(label='Particle Density (Fixed Scale)')
        plt.title(f"Step {step} - 2D DSMC with NTC Collisions")
        plt.xlabel("X (m)"); plt.ylabel("Y (m)")
        plt.savefig(f'frames_2d/step_{step:04d}.jpg')
        plt.close()
        print(f"Step {step} | Particles: {len(pos)}")

print("Simulation complete. Images saved in 'frames_2d' folder.")
