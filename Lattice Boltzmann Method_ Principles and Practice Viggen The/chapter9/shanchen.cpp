/* This code accompanies
 *   The Lattice Boltzmann Method: Principles and Practice
 *   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
 *   ISBN 978-3-319-44649-3 (Electronic) 
 *        978-3-319-44647-9 (Print)
 *   http://www.springer.com/978-3-319-44647-9
 *
 * This code is provided under the MIT license. See LICENSE.txt.
 *
 * Author: Timm Krüger
 *
 */
#include <iostream>
#include <cmath>
#include <time.h>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <algorithm>

using namespace std;

// Lattice parameters
const int nx = 64; // number of nodes along x-axis
const int ny = 64; // number of nodes along y-axis
const int nsteps = 20000; // number of time steps
const int noutput = 100; // data output interval (data written to "/data")
const int nfluids = 1; // number of fluid components (choose 1 or 2)
const double tauA = 1; // relaxation time for fluid A
const double tauB = 1; // relaxation time for fluid B (will only be used if nfluids = 2)

// Shan-Chen parameters
const double gA = -4.7; // self-interaction strength of fluid A
const double gB = 0.; // self-interaction strength of fluid B (will only be used if nfluids = 2)
const double gAB = 6.; // interaction strength of fluids A and B (will only be used if nfluids = 2)
const double rho0 = 1.; // reference density for pseudopotential (usually set to 1)
const double rhol = 2.1; // initial liquid density
const double rhog = 0.15; // initial gas density
const double radius = 15.; // initial droplet radius

// Fixed parameters; DO NOT CHANGE
const int npop = 9; // number of populations
const int cx[] = {0, 1, 0, -1, 0, 1, -1, -1, 1}; // x-components of lattice vectors
const int cy[] = {0, 0, 1,  0,-1, 1,  1, -1,-1}; // y-components of lattice vectors
const double weight[]={4./9., 1./9., 1./9., 1./9., 1./9., 1./36., 1./36., 1./36., 1./36.};

// Arrays
double g[nfluids][nfluids]; // interaction strengths
double rho[nfluids][nx * ny]; // density
double press[nx * ny]; // pressure
double ux[nx * ny]; // x-component of fluid velocity
double uy[nx * ny]; // y-component of fluid velocity
double Fx[nfluids][nx * ny]; // x-component of Shan-Chen force
double Fy[nfluids][nx * ny]; // y-component of Shan-Chen force
double feq[nfluids][npop]; // equilibrium populations
double forcing[nfluids][npop]; // force populations
double tau[nfluids]; // relaxation times
double **f1; // populations (old)
double **f2; // populations (new)

// Compute density everywhere.
void computeDensity(double **pop) {
  for (int k = 0; k < nx * ny; k++) {
    for (int s = 0; s < nfluids; s++) {
      rho[s][k] = pop[s][npop * k] + pop[s][npop * k + 1] + pop[s][npop * k + 2] + pop[s][npop * k + 3] + pop[s][npop * k + 4] + pop[s][npop * k + 5] + pop[s][npop * k + 6] + pop[s][npop * k + 7] + pop[s][npop * k + 8];
    }
  }

  return;
}

// Compute pseudopotential for given density value.
double psi(double dens) {
  return rho0 * (1. - exp(-dens / rho0));
}

// Compute Shan-Chen forces.
void computeSCForces() {
  for (int y = 0; y < ny; y++) {
    for (int x = 0; x < nx; x++) {
      const int k = y * nx + x;
      for (int s = 0; s < nfluids; s++) {
        Fx[s][k] = 0.;
        Fy[s][k] = 0.;
        for (int ss = 0; ss < nfluids; ss++) {
          double fxtemp = 0.;
          double fytemp = 0.;

          for (int i = 1; i < npop; i++) {
            const int x2 = (x + cx[i] + nx) % nx;
            const int y2 = (y + cy[i] + ny) % ny;
            const double psinb = psi(rho[ss][y2 * nx + x2]);
            fxtemp += weight[i] * cx[i] * psinb;
            fytemp += weight[i] * cy[i] * psinb;
          }

          const double psiloc = psi(rho[s][k]);
          fxtemp *= (-g[s][ss] * psiloc);
          fytemp *= (-g[s][ss] * psiloc);
          Fx[s][k] += fxtemp;
          Fy[s][k] += fytemp;
        }
      }
    }
  }

  return;
}

// Calculate total density at given point.
double computeTotalDensity(int k) {
  double dens;
  
  if (nfluids == 1) {
    dens = rho[0][k];
  }
  else if (nfluids == 2) {
    dens = rho[0][k] + rho[1][k];
  }

  return dens;
}

// Calculate pressure everywhere.
void computePressure() {
  for (int y = 0; y < ny; y++) {
    for (int x = 0; x < nx; x++) {
      const int k = y * nx + x;
      press[k] = 0.;
      for (int s = 0; s < nfluids; s++) {
        press[k] += rho[s][k] / 3.;
        for (int ss = 0; ss < nfluids; ss++) {
          press[k] += (g[s][ss] * psi(rho[s][k]) * psi(rho[ss][k]) / 6.);
        }
      }
    }
  }
}

// Calculate barycentric velocity everywhere.
void computeVelocity(double **pop) {
  for (int y = 0; y < ny; y++) {
    for (int x = 0; x < nx; x++) {
      const int k = y * nx + x;
      ux[k] = 0.;
      uy[k] = 0.;
      for (int s = 0; s < nfluids; s++) {
        ux[k] += (pop[s][npop * k + 1] - pop[s][npop * k + 3] + pop[s][npop * k + 5] - pop[s][npop * k + 6] - pop[s][npop * k + 7] + pop[s][npop * k + 8]);
        uy[k] += (pop[s][npop * k + 2] - pop[s][npop * k + 4] + pop[s][npop * k + 5] + pop[s][npop * k + 6] - pop[s][npop * k + 7] - pop[s][npop * k + 8]);
        ux[k] += (0.5 * Fx[s][k]);
        uy[k] += (0.5 * Fy[s][k]);
      }
      const double dens = computeTotalDensity(k);
      ux[k] /= dens;
      uy[k] /= dens;
    }
  }

  return;
}

// Compute equilibrium distributions for fluid s at point k.
void equilibrium(int s, int k) {
  const double dens = rho[s][k];
  const double vx = ux[k];
  const double vy = uy[k];
  const double usq = vx * vx + vy * vy;
  feq[s][0] = weight[0] * dens * (1. - 1.5 * usq);
  feq[s][1] = weight[1] * dens * (1. + 3. * vx + 4.5 * vx * vx - 1.5 * usq);
  feq[s][2] = weight[2] * dens * (1. + 3. * vy + 4.5 * vy * vy - 1.5 * usq);
  feq[s][3] = weight[3] * dens * (1. - 3. * vx + 4.5 * vx * vx - 1.5 * usq);
  feq[s][4] = weight[4] * dens * (1. - 3. * vy + 4.5 * vy * vy - 1.5 * usq);
  feq[s][5] = weight[5] * dens * (1. + 3. * ( vx + vy) + 4.5 * ( vx + vy) * ( vx + vy) - 1.5 * usq);
  feq[s][6] = weight[6] * dens * (1. + 3. * (-vx + vy) + 4.5 * (-vx + vy) * (-vx + vy) - 1.5 * usq);
  feq[s][7] = weight[7] * dens * (1. + 3. * (-vx - vy) + 4.5 * ( vx + vy) * ( vx + vy) - 1.5 * usq);
  feq[s][8] = weight[8] * dens * (1. + 3. * ( vx - vy) + 4.5 * ( vx - vy) * ( vx - vy) - 1.5 * usq);

  return;
}

// Initialise simulation
void initialisation(double **f1, double **f2) {
  // Set viscosity
  tau[0] = tauA;

  if (nfluids == 2) {
    tau[1] = tauB;
  }

  // Set interaction strength
  g[0][0] = gA;
  
  if (nfluids == 2) {
    g[1][1] = gB;
    g[0][1] = g[1][0] = gAB;
  }

  // Initialise 1-component system.
  // In this case a liquid droplet is created in a gas.
  if (nfluids == 1) {
    for (int k = 0; k < nx * ny; k++) {
      // Set initial density field (liquid density inside, gas density outside)
      if ((k / nx - ny / 2.) * (k / nx - ny / 2.) + (k % nx - nx / 2.) * (k % nx - nx / 2.) <= radius * radius) {
        rho[0][k] = rhol; // liquid density
      }
      else {
        rho[0][k] = rhog; // gas density
      }
    }
  }
  
  // Initialise 2-component system.
  // In this case a planar interface is created, and the total density is set to unity.
  if (nfluids == 2) {
    for (int y = 0; y < ny; y++) {
      for (int x = 0; x < nx; x++) {
        const int k = y * nx + x;
        if (y < ny / 2.) {
          rho[0][k] = 0.1;
          rho[1][k] = 0.9;
        }
        else {
          rho[0][k] = 0.9;
          rho[1][k] = 0.1;          
        }
      }
    }
  }

  // Initialise populations.
  for (int s = 0; s < nfluids; s++) {
    for (int k = 0; k < nx * ny; k++) {
      // Set initial velocity field (zero velocity everywhere).
      ux[k] = uy[k] = 0.;

      // Compute equilibrium distributions
      equilibrium(s, k);

      // Set populations to equilibrium
      for (int i = 0; i < npop; i++) {
        f1[s][npop * k + i] = feq[s][i];
        f2[s][npop * k + i] = feq[s][i];
      }
    }
  }
  
  return;
}

// LBM: collide and propagate (push).
void push(double **f1, double **f2) {
  for (int s = 0; s < nfluids; s++) {
    const double omega = 1. / tau[s];

    for (int y = 0; y < ny; y++) {
      for (int x = 0; x < nx; x++) {
        const int k = y * nx + x;

        // Compute Guo's forcing terms
        for (int i = 0; i < npop; i++) {
          forcing[s][i] = weight[i] * (1. - 0.5 * omega) * ((3. * (cx[i] - ux[k]) + 9. * cx[i] * (cx[i] * ux[k] + cy[i] * uy[k])) * Fx[s][k] + (3. * (cy[i] - uy[k]) + 9. * cy[i] * (cx[i] * ux[k] + cy[i] * uy[k])) * Fy[s][k]);
        }

        // Compute equilibrium distributions
        equilibrium(s, k);

        // Collide and propagate
        for (int i = 0; i < npop; i++)
        {
          int x2 = (x + cx[i] + nx) % nx;
          int y2 = (y + cy[i] + ny) % ny;
          f2[s][npop * (y2 * nx + x2) + i] = f1[s][npop * k + i] * (1. - omega) + feq[s][i] * omega + forcing[s][i];
        }
      }
    }
  }
  
  return;
}

// Helper function for writing data to disk.
void writeFile(string name, double* data, int n)
{
  name = "data/" + name;
  ofstream fout(name.c_str());
  for (int y = 0; y < ny; y++) {
    for (int x = 0; x < nx; x++) {
      fout << data[y * nx + x] << " ";
    }
    fout << "\n";
  }
  fout << endl;

  return;
}

// Write profiles to disk.
void writeProfiles(int step)
{
  stringstream denstream;
  stringstream pressstream;
  stringstream velxstream;
  stringstream velystream;
  stringstream len;
  len << step;

  pressstream.str("");
  velxstream.str("");
  velystream.str("");
  pressstream << "press_" << string(5 - len.str().size(), '0') << step << ".dat";
  velxstream << "velx_" << string(5 - len.str().size(), '0') << step << ".dat";
  velystream << "vely_" << string(5 - len.str().size(), '0') << step << ".dat";
  writeFile(pressstream.str(), press, nx * ny);
  writeFile(velxstream.str(), ux, nx * ny);
  writeFile(velystream.str(), uy, nx * ny);
 
  for (int s = 0; s < nfluids; s++) {
    denstream.str("");
    denstream << "density_comp" << s << "_" << string(5 - len.str().size(), '0' ) << step << ".dat";
    writeFile(denstream.str(), rho[s], nx * ny);
  }
}

// Compute Laplace pressure.
void calculateDeltaPressure() {
  double rho_gas = 0.;
  double rho_liq = 0.;
  double press_gas = 0.;
  double press_liq = 0.;

  // Average gas pressure in a square box with 4 lattice nodes
  for (int x = 0; x < 4; x++) {
    for (int y = 0; y < 4; y++) {
      const int k = y * nx + x;
      rho_gas += rho[0][k];
      press_gas += press[k];
    }
  }

  // Average liquid pressure in a square box with 4 lattice nodes
  for (int x = nx / 2 - 2; x < nx / 2 + 2; x++) {
    for (int y = ny / 2 - 2; y < ny / 2 + 2; y++) {
      const int k = y * nx + x;
      rho_liq += rho[0][k];
      press_liq += press[k];
    }
  }

  rho_gas /= 16.;
  rho_liq /= 16.;
  press_gas /= 16.;
  press_liq /= 16.;
  
  const double delta_press = press_liq - press_gas;
  const double rho_av = 0.5 * (rho_gas + rho_liq);
  const int y = ny / 2;
  double rad;
  
  for (int x = 0; x < nx / 2; x++) {
    if (rho[0][y * nx + x] > rho_av) {
      const double drho = rho[0][y * nx + x] - rho[0][y * nx + (x - 1)];
      const double dx = (rho_av - rho[0][y * nx + (x - 1)]) / drho;
      rad = (nx / 2. - x) + (1. - dx);
      break;
    }
  }
  
  const double gamma = delta_press * rad;

  cout << "  Densities: rho_g = " << rho_gas << ", rho_l = " << rho_liq << "\n";
  cout << "  Pressure difference: dp = " << delta_press << "\n";
  cout << "  Droplet radius: r = " << rad << "\n";
  cout << "  Surface tension: gamma = " << gamma << endl;
  
  return;
}

// Main function.
int main(int argc, char** argv) {
  // Set simulation timers
  time_t start, finish;
  start = time(NULL);

  // Allocate memory for populations
  double **f1 = new double*[nfluids]; // old populations
  double **f2 = new double*[nfluids]; // new populations

  for(int s = 0; s < nfluids; s++) {
    f1[s] = new double[nx * ny * npop];
    f2[s] = new double[nx * ny * npop];
  }

  // Initialise domain
  initialisation(f1, f2);

  // Main loop
  for (int step = 0; step <= nsteps; step++) {
    // Calculate density field.
    computeDensity(f1);
    
    // Calculate SC forces.
    computeSCForces();
    
    // Calculate barycentric velocity.
    computeVelocity(f1);
    
    // Collide and propagate.
    push(f1, f2);

    // Swap old and new populations.
    swap(f1, f2);
    
    // Write data to disk/console
    if (step % noutput == 0) {
      cout << "Running time step " << step << endl;
      computePressure();
      writeProfiles(step);
      calculateDeltaPressure();      
    }
  }

  finish = time(NULL);

  cout << "Overall CPU time required: " << finish - start << " sec" << endl;
  cout << "This corresponds to " << nx * ny * nsteps / (finish - start) / 1000000. << " MLUPS" << endl;

  return 0;
}
