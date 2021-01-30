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

/* This is an example 2D immersed boundary lattice Boltzmann method code.
 * It uses the D2Q9 lattice with Guo's forcing term.
 * The purpose is to simulate Poiseuille flow with rigid walls simulated by IBM nodes.
 * Rigid bottom and top walls are parallel to the x-axis (channel).
 * The flow is periodic along the x-axis.
 *
 * The lattice velocities are defined according to the following scheme:
 * index:   0  1  2  3  4  5  6  7  8
 * ----------------------------------
 * x:       0 +1 -1  0  0 +1 -1 +1 -1
 * y:       0  0  0 +1 -1 +1 -1 -1 +1
 *
 * 8 3 5  ^y
 *  \|/   |   x
 * 2-0-1   --->
 *  /|\
 * 6 4 7
 *
 */

// *********************
// PREPROCESSOR COMMANDS
// *********************

#include <vector>   // vector containers
#include <cmath>    // mathematical library
#include <iostream> // for the use of 'cout'
#include <fstream>  // file streams
#include <sstream>  // string streams
#include <cstdlib>  // standard library

#define SQ(x) ((x) * (x)) // square function; replaces SQ(x) by ((x) * (x)) in the code

using namespace std;

// *********************
// SIMULATION PARAMETERS
// *********************

// These are the relevant simulation parameters that can be changed by the user.
// If a bottom or top wall shall move in negative x-direction, a negative velocity has to be specified.
// Moving walls and gravity can be switched on simultaneously.

// Basic fluid/lattice properties
const int Nx = 19; // number of lattice nodes along the x-axis (periodic)
const int Ny = 20; // number of lattice nodes along the y-axis (including two wall nodes)
const double tau = 1.; // relaxation time
const int t_num = 2000; // number of time steps (running from 1 to t_num)
const int t_disk = 100; // disk write time step (data will be written to the disk every t_disk step)
const int t_info = 100; // info time step (screen message will be printed every t_info step)
const double gravity = 1.e-5; // force density due to gravity (in positive x-direction)
const double vel_bot = 0.; // velocity of the bottom wall (in positive x-direction)
const double vel_top = 0.; // velocity of the top wall (in positive x-direction)

// IBM properties
// Note that bounce-back is used for the outermost lattice nodes, so the width of the channel "wall_width" should be smaller than Ny - 2.
int IBM_stencil = 2; // interpolation stencil; available values: 2, 4
const int wall_num_nodes = 19; // number of IBM nodes along each wall
const double wall_width = 15.3; // channel width
const double wall_stiffness = 1; // IBM spring constant

// ******************
// PARTICLE STRUCTURE
// ******************

// Structure for IBM nodes

struct node_struct {

  // Constructor
  node_struct() {
   x = 0.;
   y = 0.;
   x_ref = 0.;
   y_ref = 0.;
   vel_x = 0.;
   vel_y = 0.;
   force_x = 0.;
   force_y = 0.;
  }

  // Elements
  double x; // current x-position
  double y; // current y-position
  double x_ref; // reference x-position
  double y_ref; // reference y-position
  double vel_x; // node velocity (x-component)
  double vel_y; // node velocity (y-component)
  double force_x; // node force (x-component)
  double force_y; // node force (y-component)
};

// Structure for immersed objects

struct IBM_object {
  
  // Constructor
  IBM_object(int number) {
    num_nodes = number;
    node = new node_struct[num_nodes];
  }
  
  // Elements
  bool is_wall;
  int num_nodes; // number of surface nodes
  double radius; // object radius
  double stiffness; // stiffness modulus
  node_struct *node; // list of nodes
};

// ***************************************
// DECLARE ADDITIONAL VARIABLES AND ARRAYS
// ***************************************

const double omega = 1. / tau; // relaxation frequency (inverse of relaxation time)
double ***pop, ***pop_old; // LBM populations (old and new)
double **density; // fluid density
double **velocity_x; // fluid velocity (x-component)
double **velocity_y; // fluid velocity (y-component)
double **force_x; // fluid force (x-component)
double **force_y; // fluid force (y-component)
double force_latt[9]; // lattice force term entering the lattice Boltzmann equation
double pop_eq[9]; // equilibrium populations
const double weight[9] = {4./9., 1./9., 1./9., 1./9., 1./9., 1./36., 1./36., 1./36., 1./36.}; // lattice weights
vector<IBM_object> boundaries; // immersed boundary objects

// *****************
// DECLARE FUNCTIONS
// *****************

void initialize(); // allocate memory and initialize variables
void LBM(); // perform LBM operations
void momenta(); // compute fluid density and velocity from the populations
void equilibrium(double, double, double); // compute the equilibrium populations from the fluid density and velocity
void compute_particle_forces(vector<IBM_object>&); // compute the forces acting on the object nodes
void spread(vector<IBM_object>&); // spread node forces to fluid lattice
void interpolate(vector<IBM_object>&); // interpolate node velocities from fluid velocity
double stencil(double); // compute IBM interpolation stencil
void update_particle_position(vector<IBM_object>&); // update node positions
void write_fluid_vtk(int); // write the fluid state to the disk as VTK file
void write_particle_vtk(int, vector<IBM_object>&); // write the particle state to the disk as VTK file
void write_fluid_profile(int); // write lattice velocity profile to disk

// *************
// MAIN FUNCTION
// *************

// This is the main function, containing the simulation initialization and the simulation loop.
// Overview of simulation algorithm:
// 1) compute the node forces based on the object's deformation
// 2) spread the node forces to the fluid lattice
// 3) update the fluid state via LBM
// 4) interpolate the fluid velocity to the object nodes
// 5) update node positions
// 6) if desired, write data to disk and report status

int main() {

  initialize(); // allocate memory and initialize variables

  cout << "Starting simulation:" << endl;

  for(int t = 1; t <= t_num; ++t) { // run over all times between 1 and t_num
    interpolate(boundaries); // interpolate velocity
    update_particle_position(boundaries); // update particle position
    compute_particle_forces(boundaries); // compute particle forces
    spread(boundaries); // spread forces from the Lagrangian to the Eulerian mesh
    LBM(); // perform collision, propagation, and bounce-back
    
    // Write fluid and particle to VTK files
    if(t % t_disk == 0) {
      cout << "  Writing data at " << t << endl;
      write_fluid_vtk(t);
      write_particle_vtk(t, boundaries);
      write_fluid_profile(t);
    }

    // Report end of time step
    if(t % t_info == 0) {
      cout << "  Completed time step " << t << " in [1, " << t_num << "]" << endl;
    }
  }

  // Report successful end of simulation
  cout << "Simulation complete." << endl;

  return 0;
}

// ****************************************
// ALLOCATE MEMORY AND INITIALIZE VARIABLES
// ****************************************

// The memory for lattice variables (populations, density, velocity, forces) is allocated.
// The variables are initialized.

void initialize() {

  // Create folders, delete data file
  // Make sure that the VTK folders exist.
  // Old file data.dat is deleted, if existing.
  int ignore; // ignore return value of system calls
  ignore = system("mkdir -p vtk"); // create folder if not existing
  ignore = system("mkdir -p data"); // create folder if not existing
  ignore = system("rm -f data.dat"); // delete file if existing
  
  // Compute derived quantities.
  const double D = wall_width; // inner channel diameter
  const double nu = (tau - 0.5) / 3; // viscosity
  const double umax = gravity / (2 * nu) * SQ(0.5 * D); // maximum velocity in Poiseuille flow
  const double Re = D * umax / nu; // Reynolds number for Poiseuille flow
  cout << "Computing derived quantities:" << endl;
  cout << "  Viscosity: " << nu << endl;
  cout << "  Expected maximum velocity for Poiseuille flow: " << umax << endl;
  cout << "  Reynolds number for Poiseuille flow: " << Re << endl;
  
  // Allocate memory for the fluid density, velocity, and force
  density = new double*[Nx];
  velocity_x = new double*[Nx];
  velocity_y = new double*[Nx];
  force_x = new double*[Nx];
  force_y = new double*[Nx];

  for(int X = 0; X < Nx; ++X) {
    density[X] = new double[Ny];
    velocity_x[X] = new double[Ny];
    velocity_y[X] = new double[Ny];
    force_x[X] = new double[Ny];
    force_y[X] = new double[Ny];
  }

  // Initialize the fluid density and velocity. Start with unit density and zero velocity.
  for(int X = 0; X < Nx; ++X) {
    for(int Y = 0; Y < Ny; ++Y) {
      density[X][Y] = 1;
      velocity_x[X][Y] = 0;
      velocity_y[X][Y] = 0;
      force_x[X][Y] = 0;
      force_y[X][Y] = 0;
    }
  }

  // Allocate memory for the populations
  pop = new double**[9];
  pop_old = new double**[9];

  for(int c_i = 0; c_i < 9; ++c_i) {
    pop[c_i] = new double*[Nx];
    pop_old[c_i] = new double*[Nx];

    for(int X = 0; X < Nx; ++X) {
      pop[c_i][X] = new double[Ny];
      pop_old[c_i][X] = new double[Ny];

      for(int Y = 0; Y < Ny; ++Y) {
        pop[c_i][X][Y] = 0;
        pop_old[c_i][X][Y] = 0;
      }
    }
  }

  // Initialize the populations. Use the equilibrium populations corresponding to the initialized fluid density and velocity.
  for(int X = 0; X < Nx; ++X) {
    for(int Y = 0; Y < Ny; ++Y) {
      equilibrium(density[X][Y], velocity_x[X][Y], velocity_y[X][Y]);

      for(int c_i = 0; c_i < 9; ++c_i) {
        pop_old[c_i][X][Y] = pop_eq[c_i];
        pop[c_i][X][Y] = pop_eq[c_i];
      }
    }
  }

  // Allocate memory for immersed boundaries and initialize
  boundaries.push_back(2 * wall_num_nodes);
  boundaries[boundaries.size() - 1].is_wall = true;
  boundaries[boundaries.size() - 1].stiffness = wall_stiffness;
  
  for(int n = 0; n < wall_num_nodes; ++n) {
    boundaries[boundaries.size() - 1].node[n].x = (Nx * (n + 0.5)) / wall_num_nodes;
    boundaries[boundaries.size() - 1].node[n].x_ref = (Nx * (n + 0.5)) / wall_num_nodes;
    boundaries[boundaries.size() - 1].node[n + wall_num_nodes].x = (Nx * (n + 0.5)) / wall_num_nodes;
    boundaries[boundaries.size() - 1].node[n + wall_num_nodes].x_ref = (Nx * (n + 0.5)) / wall_num_nodes;
    boundaries[boundaries.size() - 1].node[n].y = (Ny - 2) / 2. - wall_width / 2.;
    boundaries[boundaries.size() - 1].node[n].y_ref = (Ny - 2) / 2. - wall_width / 2.;
    boundaries[boundaries.size() - 1].node[n + wall_num_nodes].y = (Ny - 2) / 2. + wall_width / 2.;
    boundaries[boundaries.size() - 1].node[n + wall_num_nodes].y_ref = (Ny - 2) / 2. + wall_width / 2.;
  }

  return;
}

// *******************
// COMPUTE EQUILIBRIUM
// *******************

// This function computes the equilibrium populations from the fluid density and velocity.
// It computes the equilibrium only at a specific lattice node: Function has to be called at each lattice node.
// The standard quadratic equilibrium is used.

void equilibrium(double den, double vel_x, double vel_y) {
  pop_eq[0] = weight[0] * den * (1                                                     - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[1] = weight[1] * den * (1 + 3 * (  vel_x        ) + 4.5 * SQ(  vel_x        ) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[2] = weight[2] * den * (1 + 3 * (- vel_x        ) + 4.5 * SQ(- vel_x        ) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[3] = weight[3] * den * (1 + 3 * (          vel_y) + 4.5 * SQ(          vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[4] = weight[4] * den * (1 + 3 * (        - vel_y) + 4.5 * SQ(        - vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[5] = weight[5] * den * (1 + 3 * (  vel_x + vel_y) + 4.5 * SQ(  vel_x + vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[6] = weight[6] * den * (1 + 3 * (- vel_x - vel_y) + 4.5 * SQ(- vel_x - vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[7] = weight[7] * den * (1 + 3 * (  vel_x - vel_y) + 4.5 * SQ(  vel_x - vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));
  pop_eq[8] = weight[8] * den * (1 + 3 * (- vel_x + vel_y) + 4.5 * SQ(- vel_x + vel_y) - 1.5 * (SQ(vel_x) + SQ(vel_y)));

  return;
}

// **********************
// PERFORM LBM OPERATIONS
// **********************

void LBM() {

  // The code uses old and new populations which are swapped at the beginning of each time step.
  // This way, the old populations are not overwritten during propagation.

  double ***swap_temp = pop_old;
  pop_old = pop;
  pop = swap_temp;

  // The lattice Boltzmann equation is solved in the following.
  // The algorithm includes
  // - computation of the lattice force
  // - combined collision and propagation (push)
  // - half-way bounce-back (the outermost nodes are solid nodes)

  for(int X = 0; X < Nx; ++X) {
    for(int Y = 1; Y < Ny - 1; ++Y) {
      // Compute the force-shifted velocity.
      const double vel_x = velocity_x[X][Y] + 0.5 * (force_x[X][Y] + gravity) / density[X][Y];
      const double vel_y = velocity_y[X][Y] + 0.5 * force_y[X][Y] / density[X][Y];

      // Compute lattice force (Guo's forcing). Gravity is always along the x-axis.
      force_latt[0] = (1 - 0.5 * omega) * weight[0] * (3 * ((   - vel_x) * (force_x[X][Y] + gravity) + (   - vel_y) * force_y[X][Y]));
      force_latt[1] = (1 - 0.5 * omega) * weight[1] * (3 * (( 1 - vel_x) * (force_x[X][Y] + gravity) + (   - vel_y) * force_y[X][Y]) + 9 * (vel_x) * (force_x[X][Y] + gravity));
      force_latt[2] = (1 - 0.5 * omega) * weight[2] * (3 * ((-1 - vel_x) * (force_x[X][Y] + gravity) + (   - vel_y) * force_y[X][Y]) + 9 * (vel_x) * (force_x[X][Y] + gravity));
      force_latt[3] = (1 - 0.5 * omega) * weight[3] * (3 * ((   - vel_x) * (force_x[X][Y] + gravity) + ( 1 - vel_y) * force_y[X][Y]) + 9 * (vel_y) * force_y[X][Y]);
      force_latt[4] = (1 - 0.5 * omega) * weight[4] * (3 * ((   - vel_x) * (force_x[X][Y] + gravity) + (-1 - vel_y) * force_y[X][Y]) + 9 * (vel_y) * force_y[X][Y]);
      force_latt[5] = (1 - 0.5 * omega) * weight[5] * (3 * (( 1 - vel_x) * (force_x[X][Y] + gravity) + ( 1 - vel_y) * force_y[X][Y]) + 9 * (vel_x + vel_y) * (force_x[X][Y] + gravity + force_y[X][Y]));
      force_latt[6] = (1 - 0.5 * omega) * weight[6] * (3 * ((-1 - vel_x) * (force_x[X][Y] + gravity) + (-1 - vel_y) * force_y[X][Y]) + 9 * (vel_x + vel_y) * (force_x[X][Y] + gravity + force_y[X][Y]));
      force_latt[7] = (1 - 0.5 * omega) * weight[7] * (3 * (( 1 - vel_x) * (force_x[X][Y] + gravity) + (-1 - vel_y) * force_y[X][Y]) + 9 * (vel_x - vel_y) * (force_x[X][Y] + gravity - force_y[X][Y]));
      force_latt[8] = (1 - 0.5 * omega) * weight[8] * (3 * ((-1 - vel_x) * (force_x[X][Y] + gravity) + ( 1 - vel_y) * force_y[X][Y]) + 9 * (vel_x - vel_y) * (force_x[X][Y] + gravity - force_y[X][Y]));

      // Compute equilibrium populations.
      equilibrium(density[X][Y], vel_x, vel_y);

      // This is the lattice Boltzmann equation (combined collision and propagation) including external forcing.
      // Periodicity of the lattice in x-direction is taken into account by the %-operator.
      pop[0][X]                [Y]     = pop_old[0][X][Y] * (1. - omega) + pop_eq[0] * omega + force_latt[ 0];
      pop[1][(X + 1) % Nx]     [Y]     = pop_old[1][X][Y] * (1. - omega) + pop_eq[1] * omega + force_latt[ 1];
      pop[2][(X - 1 + Nx) % Nx][Y]     = pop_old[2][X][Y] * (1. - omega) + pop_eq[2] * omega + force_latt[ 2];
      pop[3][X]                [Y + 1] = pop_old[3][X][Y] * (1. - omega) + pop_eq[3] * omega + force_latt[ 3];
      pop[4][X]                [Y - 1] = pop_old[4][X][Y] * (1. - omega) + pop_eq[4] * omega + force_latt[ 4];
      pop[5][(X + 1) % Nx]     [Y + 1] = pop_old[5][X][Y] * (1. - omega) + pop_eq[5] * omega + force_latt[ 5];
      pop[6][(X - 1 + Nx) % Nx][Y - 1] = pop_old[6][X][Y] * (1. - omega) + pop_eq[6] * omega + force_latt[ 6];
      pop[7][(X + 1) % Nx]     [Y - 1] = pop_old[7][X][Y] * (1. - omega) + pop_eq[7] * omega + force_latt[ 7];
      pop[8][(X - 1 + Nx) % Nx][Y + 1] = pop_old[8][X][Y] * (1. - omega) + pop_eq[8] * omega + force_latt[ 8];
    }
  }

  // Bounce-back
  // Due to the presence of the rigid walls at y = 0 and y = Ny - 1, the populations have to be bounced back.
  // Ladd's momentum correction term is included for moving walls (wall velocity parallel to x-axis).
  // Periodicity of the lattice in x-direction is taken into account via the %-operator.

  for(int X = 0; X < Nx; ++X) {

    // Bottom wall (y = 0)
    pop[3][X][1] = pop[4][X]                [0];
    pop[5][X][1] = pop[6][(X - 1 + Nx) % Nx][0] + 6 * weight[6] * density[X][1] * vel_bot;
    pop[8][X][1] = pop[7][(X + 1) % Nx]     [0] - 6 * weight[7] * density[X][1] * vel_bot;

    // Top wall (y = Ny - 1)
    pop[4][X][Ny - 2] = pop[3][X]                [Ny - 1];
    pop[6][X][Ny - 2] = pop[5][(X + 1) % Nx]     [Ny - 1] - 6 * weight[5] * density[X][Ny - 2] * vel_top;
    pop[7][X][Ny - 2] = pop[8][(X - 1 + Nx) % Nx][Ny - 1] + 6 * weight[8] * density[X][Ny - 2] * vel_top;
  }

  // The new fluid density and velocity are obtained from the populations.
  momenta();

  return;
}

// **********************************
// COMPUTE FLUID DENSITY AND VELOCITY
// **********************************

// This function computes the fluid density and velocity from the populations.
// The velocity correction due to body force is *not* included here.

void momenta() {
  for(int X = 0; X < Nx; ++X) {
    for(int Y = 1; Y < Ny - 1; ++Y) {
      density[X][Y] = pop[0][X][Y] + pop[1][X][Y] + pop[2][X][Y] + pop[3][X][Y] + pop[4][X][Y] + pop[5][X][Y] + pop[6][X][Y] + pop[7][X][Y] + pop[8][X][Y];
      velocity_x[X][Y] = (pop[1][X][Y] - pop[2][X][Y] + pop[5][X][Y] - pop[6][X][Y] + pop[7][X][Y] - pop[8][X][Y]) / density[X][Y];
      velocity_y[X][Y] = (pop[3][X][Y] - pop[4][X][Y] + pop[5][X][Y] - pop[6][X][Y] - pop[7][X][Y] + pop[8][X][Y]) / density[X][Y];
    }
  }

  return;
}

// ******************
// COMPUTE IBM FORCES
// ******************

// The forces acting on the IBM nodes are computed.

void compute_particle_forces(vector<IBM_object> & boundary) {

  // Run over all boundaries.
  for(int i = 0; i < boundary.size(); ++i) {
    // Reset forces. This way, the force from the previous time step is deleted.
    // This is necessary whenever forces are computed using '+='.

    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      boundary[i].node[n].force_x = 0.;
      boundary[i].node[n].force_y = 0.;
    }

    // Compute area belonging to a node.
    // The area is used to weight the penalty force; this way the total force is independent of the number of IBM nodes.
    const double area = (double) Nx / boundary[i].num_nodes;

    // Compute rigid forces (node forces are proportional to their displacements).
    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      boundary[i].node[n].force_x = -boundary[i].stiffness * (boundary[i].node[n].x - boundary[i].node[n].x_ref) * area;
      boundary[i].node[n].force_y = -boundary[i].stiffness * (boundary[i].node[n].y - boundary[i].node[n].y_ref) * area;
    }
  }

  return;
}

// *************
// SPREAD FORCES
// *************

// The node forces are spread to the fluid nodes via IBM.
// The two- and four-point interpolation stencils are used in the present code.

void spread(vector<IBM_object> & boundary) {

  int X_s, X_e, Y_s, Y_e;
  
  // Reset forces because '+=' is used.
  for(int X = 0; X < Nx; ++X) {
    for(int Y = 0; Y < Ny; ++Y) {
      force_x[X][Y] = 0.;
      force_y[X][Y] = 0.;
    }
  }

  // Run over all boundaries.
  for(int i = 0; i < boundary.size(); ++i) {
  
    // Run over all boundary nodes.
    for(int n = 0; n < boundary[i].num_nodes; ++n) {

      // Identify the lowest fluid lattice node in interpolation range. 'Lowest' means: its x- and y-values are the smallest.
      // The other fluid nodes in range have coordinates (x_int + 1, y_int), (x_int, y_int + 1), and (x_int + 1, y_int + 1).
      int x_int = (int) (boundary[i].node[n].x - 0.5 + Nx) - Nx;
      int y_int = (int) (boundary[i].node[n].y + 0.5);
      if(IBM_stencil == 2) {
        X_s = x_int;
        X_e = x_int + 1;
        Y_s = y_int;
        Y_e = y_int + 1;
      }
      else if(IBM_stencil == 4) {
        X_s = x_int - 1;
        X_e = x_int + 2;
        Y_s = y_int - 1;
        Y_e = y_int + 2;
      }
      
      // Run over all neighboring fluid nodes.
      // In the case of the two-point interpolation, it is 2x2 fluid nodes.
      for(int X = X_s; X <= X_e; ++X) {
        for(int Y = Y_s; Y <= Y_e; ++Y) {

          // Compute distance between object node and fluid lattice node.
          const double dist_x = boundary[i].node[n].x - 0.5 - X;
          const double dist_y = boundary[i].node[n].y + 0.5 - Y;

          // Compute interpolation weights for x- and y-direction based on the distance.
          const double weight_x = stencil(dist_x);
          const double weight_y = stencil(dist_y);

          // Compute lattice force.
          force_x[(X + Nx) % Nx][Y] += (boundary[i].node[n].force_x * weight_x * weight_y);
          force_y[(X + Nx) % Nx][Y] += (boundary[i].node[n].force_y * weight_x * weight_y);
        }
      }
    }
  }
  
  return;
}

// **********************
// INTERPOLATE VELOCITIES
// **********************

// The node velocities are interpolated from the fluid nodes via IBM.
// The two- and four-point interpolation stencils are used in the present code.

void interpolate(vector<IBM_object> & boundary) {

  int X_s, X_e, Y_s, Y_e;
  
  // Run over all boundaries.
  for(int i = 0; i < boundary.size(); ++i) {
    // Run over all boundary nodes.
    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      // Reset node velocity first since '+=' is used.
      boundary[i].node[n].vel_x = 0;
      boundary[i].node[n].vel_y = 0;

      // Identify the lowest fluid lattice node in interpolation range (see spreading).
      int x_int = (int) (boundary[i].node[n].x - 0.5 + Nx) - Nx;
      int y_int = (int) (boundary[i].node[n].y + 0.5);
      if(IBM_stencil == 2) {
        X_s = x_int;
        X_e = x_int + 1;
        Y_s = y_int;
        Y_e = y_int + 1;
      }
      else if(IBM_stencil == 4) {
        X_s = x_int - 1;
        X_e = x_int + 2;
        Y_s = y_int - 1;
        Y_e = y_int + 2;
      }

      // Run over all neighboring fluid nodes.
      // In the case of the two-point interpolation, it is 2x2 fluid nodes.
      for(int X = X_s; X <= X_e; ++X) {
        for(int Y = Y_s; Y <= Y_e; ++Y) {

          // Compute distance between object node and fluid lattice node.
          const double dist_x = boundary[i].node[n].x - 0.5 - X;
          const double dist_y = boundary[i].node[n].y + 0.5 - Y;

          // Compute interpolation weights for x- and y-direction based on the distance.
          const double weight_x = stencil(dist_x);
          const double weight_y = stencil(dist_y);
          
          // Compute node velocities.
          boundary[i].node[n].vel_x += ((velocity_x[(X + Nx) % Nx][Y] + 0.5 * (force_x[(X + Nx) % Nx][Y] + gravity) / density[(X + Nx) % Nx][Y]) * weight_x * weight_y);
          boundary[i].node[n].vel_y += ((velocity_y[(X + Nx) % Nx][Y] + 0.5 * force_y[(X + Nx) % Nx][Y] / density[(X + Nx) % Nx][Y]) * weight_x * weight_y);
        }
      }
    }
  }

  return;
}

// *******************
// COMPUTE IBM STENCIL
// *******************

// The IBM interpolation stencil is computed.

double stencil(double dist) {
 
  double stencil = 0.;
  
  if(IBM_stencil == 2) {
    stencil = 1. - abs(dist);
  }
  else if(IBM_stencil == 4) {
    if(abs(dist) < 1.) {
       stencil = 0.125 * (3. - 2 * abs(dist) + sqrt(1. + 4 * abs(dist) - 4 * SQ(dist)));
    }
    else if(abs(dist) < 2.) {
       stencil = 0.125 * (5. - 2 * abs(dist) - sqrt(-7. + 12 * abs(dist) - 4 * SQ(dist)));
    }
  }
  
  return stencil;
}

// *********************
// UPDATE NODE POSITIONS
// *********************

// The position of the particle nodes are updated according to their velocity.
// The new node position is its old position plus its current velocity (Euler integration).

void update_particle_position(vector<IBM_object> & boundary) {

  // Run over all boundaries.
  for(int i = 0; i < boundary.size(); ++i) {
    // Update node positions.
    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      boundary[i].node[n].x += boundary[i].node[n].vel_x;
      boundary[i].node[n].y += boundary[i].node[n].vel_y;
    }
  }

  return;
}

// *****************************
// WRITE FLUID STATE TO VTK FILE
// *****************************

// The fluid state is writen to a VTK file at each t_disk step.
// The following data is written:
// - density difference (density - 1)
// - x-component of velocity
// - y-component of velocity
// The following code is designed in such a way that the file can be read by ParaView.

void write_fluid_vtk(int time) {

  // Create filename.
  stringstream output_filename;
  output_filename << "vtk/fluid_t" << time << ".vtk";
  ofstream output_file;

  // Open file.
  output_file.open(output_filename.str().c_str());

  // Write VTK header.
  output_file << "# vtk DataFile Version 3.0\n";
  output_file << "fluid_state\n";
  output_file << "ASCII\n";
  output_file << "DATASET RECTILINEAR_GRID\n";
  output_file << "DIMENSIONS " << Nx << " " << Ny - 2 << " 1" << "\n";
  output_file << "X_COORDINATES " << Nx << " float\n";
  for(int X = 0; X < Nx; ++X) {
    output_file << X + 0.5 << " ";
  }

  output_file << "\n";
  output_file << "Y_COORDINATES " << Ny - 2 << " float\n";
  for(int Y = 1; Y < Ny - 1; ++Y) {
    output_file << Y - (Ny - 1) / 2. << " ";
  }
  output_file << "\n";
  output_file << "Z_COORDINATES " << 1 << " float\n";
  output_file << 0 << "\n";
  output_file << "POINT_DATA " << Nx * (Ny - 2) << "\n";

  // Write density difference.
  output_file << "SCALARS density_difference float 1\n";
  output_file << "LOOKUP_TABLE default\n";
  for(int Y = 1; Y < Ny - 1; ++Y) {
    for(int X = 0; X < Nx; ++X) {
      output_file << density[X][Y] - 1. << "\n";
    }
  }

  // Write velocity.
  output_file << "VECTORS velocity_vector float\n";
  for(int Y = 1; Y < Ny - 1; ++Y) {
    for(int X = 0; X < Nx; ++X) {
      output_file << velocity_x[X][Y] + 0.5 * (force_x[X][Y] + gravity) / density[X][Y] << " " << velocity_y[X][Y] + 0.5 * force_y[X][Y] / density[X][Y] << " 0\n";
    }
  }

  output_file.close();

  return;
}

// ****************************
// WRITE WALL STATE TO VTK FILE
// ****************************

// The node positions are writen to a VTK file at each t_disk step.
// The following code is designed in such a way that the file can be read by ParaView.

void write_particle_vtk(int time, vector<IBM_object> & boundary) {
 
  for(int i = 0; i < boundary.size(); ++i) {
    // Create filename
    stringstream output_filename;
    output_filename << "vtk/wall_t" << time << ".vtk";
    ofstream output_file;
    output_file.open(output_filename.str().c_str());

    // Write VTK header
    output_file << "# vtk DataFile Version 3.0\n";
    output_file << "wall_state\n";
    output_file << "ASCII\n";
    output_file << "DATASET POLYDATA\n";

    // Write node positions
    output_file << "POINTS " << boundary[i].num_nodes << " float\n";

    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      output_file << boundary[i].node[n].x << " " << boundary[i].node[n].y - (Ny - 2) / 2. << " 0\n";
    }

    // Write lines between neighboring nodes
    if(boundary[i].is_wall) {
      output_file << "LINES " << boundary[i].num_nodes - 2 << " " << 3 * (boundary[i].num_nodes - 2) << "\n";
      for(int n = 0; n < boundary[i].num_nodes / 2 - 1; ++n) {
        output_file << "2 " << n << " " << (n + 1) << "\n";
      }
      for(int n = boundary[i].num_nodes / 2; n < boundary[i].num_nodes - 1; ++n) {
        output_file << "2 " << n << " " << (n + 1) << "\n";
      }
    }
    else {
      output_file << "LINES " << boundary[i].num_nodes << " " << 3 * boundary[i].num_nodes << "\n";
      for(int n = 0; n < boundary[i].num_nodes; ++n) {
        output_file << "2 " << n << " " << (n + 1) % boundary[i].num_nodes << "\n";
      }
    }

    // Write vertices
    output_file << "VERTICES 1 " << boundary[i].num_nodes + 1 << "\n";
    output_file << boundary[i].num_nodes << " ";

    for(int n = 0; n < boundary[i].num_nodes; ++n) {
      output_file << n << " ";
    }

    output_file.close();
  }

  return;
}

// ************************************
// WRITE VELOCITY PROFILE TO ASCII FILE
// ************************************

void write_fluid_profile(int time) {

  // Create filename.
  stringstream output_filename;
  output_filename << "data/fluid_t" << time << ".dat";
  ofstream output_file;
  output_file.open(output_filename.str().c_str());
  
  // Write header.
  output_file << "X Y density vel_x vel_y\n";
  
  // Write data.
  for(int X = 0; X < Nx; ++X) {
    for(int Y = 0; Y < Ny; ++Y) {
      output_file << X << " " << Y << " " << density[X][Y] << " " << velocity_x[X][Y] + 0.5 * (force_x[X][Y] + gravity) / density[X][Y] << " " << velocity_y[X][Y] + 0.5 * force_y[X][Y] / density[X][Y] << "\n";
    }
  }

  output_file.close();

  return;
}

