
#include <iostream>
#include <iomanip>
#include <vector>
#include <fstream>
#include <string>
#include <sstream>
#include <regex>
#include <algorithm>
#include <Eigen/Dense>
#include <Eigen/IterativeLinearSolvers>

#include "edge.h"
#include "cell.h"

//Solver constants
const double g = 9.80665;

//Solver structure
struct sw
{
    std::fstream file;
    std::string case_name;
    double dt;
    int N_timesteps;
    int N_cells;
    int N_edges;
    std::vector<cell> cells;
    int output_frequency;
    int riemann_solver;
    int integrator;
    std::vector<edge> edges;
    std::vector<Eigen::Vector3d> vertices;
    int N_vertices;
    int implicit;
};

//Solver functions
void read_config(sw &);
void read_grid(sw &);
void find_cell_neighbours(sw &);
void write_results(sw &, int);
Eigen::Vector3d flux(Eigen::Vector3d, Eigen::Vector3d, Eigen::Vector3d, int);
Eigen::VectorXd residual(sw &, Eigen::VectorXd &);
