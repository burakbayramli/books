#include <iostream>
#include <string>
#include <chrono>

#include "sw.h"

int main(int argc, char *argv[])
{
	if (argc < 2)
	{
		std::cout << "Usage: sw_solver <case>" << std::endl;
		std::exit(-1);
	}

	// Variables
	////////////////////////////////////////////////////////////////////

	sw sw;										 //shallow water solver structure
	Eigen::VectorXd Q, Q_tmp, k1, k2, k3, k4, R; //solution and residual vectors
	Eigen::MatrixXd A;							 //matrix for system of equations (if implicit=1)
	Eigen::BiCGSTAB<Eigen::MatrixXd> solver;	 //BiCGSTAB solver (if implicit=1)
	double t;

	sw.case_name = argv[1];
	solver.setTolerance(1e-6);
	solver.setMaxIterations(20);
	t = 0.0;

	// Config
	////////////////////////////////////////////////////////////////////

	std::cout << "Reading config...";

	read_config(sw);

	std::cout << "Done!" << std::endl;

	std::cout << std::endl;
	std::cout << "dt = " << sw.dt << std::endl;
	std::cout << "N_timesteps = " << sw.N_timesteps << std::endl;
	std::cout << "output_frequency = " << sw.output_frequency << std::endl;
	std::cout << "riemann_solver = " << sw.riemann_solver << std::endl;
	std::cout << "integrator = " << sw.integrator << std::endl;
	std::cout << "implicit = " << sw.implicit << std::endl;
	std::cout << std::endl;

	// Grid
	////////////////////////////////////////////////////////////////////

	std::cout << "Reading grid...";

	read_grid(sw);

	std::cout << "Done!" << std::endl;

	std::cout << "N_vertices=" << sw.N_vertices << std::endl;
	std::cout << "N_cells=" << sw.N_cells << std::endl;
	std::cout << "N_edges=" << sw.N_edges << std::endl;

	//Initial conditions
	////////////////////////////////////////////////////////////////////

	Q = Eigen::VectorXd::Zero(3 * sw.N_cells);

	for (int i = 0; i < sw.N_cells; i++)
	{
		if (sw.cells[i].r[0] <= -0.3)
		{
			sw.cells[i].Q(0) = 1.5;
			sw.cells[i].Q(1) = 0;
			sw.cells[i].Q(2) = 0;
		}
		else
		{
			sw.cells[i].Q(0) = 1;
			sw.cells[i].Q(1) = 0;
			sw.cells[i].Q(2) = 0;
		}

		Q(3 * i) = sw.cells[i].Q(0);
		Q(3 * i + 1) = sw.cells[i].Q(1);
		Q(3 * i + 2) = sw.cells[i].Q(2);
	}

	// Jacobian (if implicit=1)
	////////////////////////////////////////////////////////////////////

	if (sw.implicit == 1)
	{
		std::cout << "Evaluating jacobian..." << std::endl;

		Eigen::VectorXd dR;

		A = Eigen::MatrixXd::Zero(3 * sw.N_cells, 3 * sw.N_cells);
		R = residual(sw, Q);

		for (int i = 0; i < 3 * sw.N_cells; i++)
		{
			Q_tmp = Q;
			Q_tmp(i) += 1e-6;
			dR = residual(sw, Q_tmp);
			for (int j = 0; j < 3 * sw.N_cells; j++)
			{
				A(j, i) = -(dR(j) - R(j)) / (1e-6);
			}
			A(i, i) += 1.0 / sw.dt;
		}

		solver.compute(A);
	}

	// Time loop
	////////////////////////////////////////////////////////////////////

	std::cout << "Performing " << sw.N_timesteps << " steps..." << std::endl;

	for (int n = 0; n < sw.N_timesteps; n++)
	{

		std::cout << "Step " << n << ", t=" << t << std::endl;

		//Residual
		R = residual(sw, Q);

		//Stop if residual is nan
		for (int i = 0; i < sw.N_cells; i++)
		{
			if (std::isnan(Q(i)))
			{
				std::cout << "Residual is nan, stopping..." << std::endl;
				std::exit(-1);
			}
		}

		if (sw.implicit == 1) //Implicit solution
		{
			Eigen::VectorXd dQ;

			dQ = solver.solve(R);

			std::cout << "Iterations:     " << solver.iterations() << std::endl;
			std::cout << "Error:     " << solver.error() << std::endl;

			Q += dQ;
		}
		else //Explicit solution
		{
			if (sw.integrator == 0) //Euler 1st-order integration
			{
				Q += sw.dt * R;
			}
			if (sw.integrator == 1) //Runge-kutta 2nd-order integration
			{
				k1 = sw.dt * R;
				Q_tmp = Q + k1 / 2.0;
				k2 = sw.dt * residual(sw, Q_tmp);
				Q += k2;
			}
			if (sw.integrator == 2) //Runge-kutta 4th-order integration
			{
				k1 = sw.dt * R;
				Q_tmp = Q + k1 / 2.0;
				k2 = sw.dt * residual(sw, Q_tmp);
				Q_tmp = Q + k2 / 2.0;
				k3 = sw.dt * residual(sw, Q_tmp);
				Q_tmp = Q + k3;
				k4 = sw.dt * residual(sw, Q_tmp);
				Q += 1.0 / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
			}
		}

		t += sw.dt;

		// Results output
		if (sw.output_frequency > 0)
		{
			if (n % sw.output_frequency == 0)
			{
				write_results(sw, n);
			}
		}
	}

	std::cout << "Done!" << std::endl;

	return 0;
}
