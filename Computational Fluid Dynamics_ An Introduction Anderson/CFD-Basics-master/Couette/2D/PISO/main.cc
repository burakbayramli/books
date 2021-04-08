#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <algorithm>
#include <cstddef>
#include <cassert>
#include <limits>
#include <Eigen/Sparse>

using namespace std;

class Array2D
{
private:
	vector<double> m_data;
	size_t m_Nx, m_Ny;

public:
	Array2D(size_t nx, size_t ny, double val = 0.0) : m_Nx(nx), m_Ny(ny), m_data(nx*ny, val) {}

	// 0-based indexing
	double &at(int i, int j)
	{
		int idx = i + m_Nx * j;
		return m_data[idx];
	}

	double at(int i, int j) const
	{
		int idx = i + m_Nx * j;
		return m_data[idx];
	}

	// 1-based indexing
	double &operator()(int i, int j)
	{
		return at(i - 1, j - 1);
	}

	double operator()(int i, int j) const
	{
		return at(i - 1, j - 1);
	}
};

const size_t WIDTH = 16;
const size_t DIGITS = 7;

const double L = 0.5; // m
const double D = 0.01; // m
const double Ue = 1.0; // m/s
const double Pe = 0.0;
const double rho = 1.225; // Kg/m3
const double mu = 3.737e-5; // Kg/m/s

const int Nx = 21, Ny = 11;
const double dx = L / (Nx - 1), dy = D / (Ny - 1);
const double dx2 = 2 * dx, dy2 = 2 * dy;
const double dxdx = dx * dx, dydy = dy * dy;
vector<double> x(Nx, 0.0), y(Ny, 0.0);

const double dt = 0.001;
double t = 0.0;
int iter_cnt = 0;
const int MAX_ITER_NUM = 2000;

const double a = 2 * (dt / dxdx + dt / dydy);
const double b = -dt / dxdx;
const double c = -dt / dydy;
double d_min = numeric_limits<double>::max(), d_max = numeric_limits<double>::min(), d_15_5 = 0.0;

Array2D p(Nx, Ny, Pe), p_star1(Nx, Ny, Pe), p_star2(Nx, Ny, 0.0);
Array2D u(Nx + 1, Ny, 0.0), u_star1(Nx + 1, Ny, 0.0), u_star2(Nx + 1, Ny, 0.0), u_wedge(Nx + 1, Ny, 0.0);
Array2D A(Nx + 1, Ny, 0.0), A_star(Nx + 1, Ny, 0.0);
Array2D v(Nx + 2, Ny + 1, 0.0), v_star1(Nx + 2, Ny + 1, 0.0), v_star2(Nx + 2, Ny + 1, 0.0), v_wedge(Nx + 2, Ny + 1, 0.0);
Array2D B(Nx + 2, Ny + 1, 0.0), B_star(Nx + 2, Ny + 1, 0.0);

// Full flowfield in TECPLOT ASCII Format.
void output1()
{
	Array2D u_interp(Nx, Ny, 0.0);
	for (int i = 1; i <= Nx; ++i)
		u_interp(i, 1) = 0.0; // Bottom
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 1; i <= Nx; ++i)
			u_interp(i, j) = (u(i, j) + u(i + 1, j)) / 2; // Inner
	for (int i = 1; i <= Nx; ++i)
		u_interp(i, Ny) = Ue; // Top

	Array2D v_interp(Nx, Ny, 0.0);
	for (int i = 1; i <= Nx; ++i)
		v_interp(i, 1) = 0.0; // Bottom
	for (int j = 2; j <= Ny - 1; ++j)
	{
		v_interp(1, j) = 0.0; // Left
		for (int i = 3; i <= Nx + 1; ++i)
			v_interp(i - 1, j) = (v(i, j) + v(i, j + 1)) / 2; // Inner and Right
	}
	for (int i = 1; i <= Nx; ++i)
		v_interp(i, Ny) = 0.0; // Top

	// Create Tecplot data file.
	ofstream result("flow" + to_string(iter_cnt) + ".dat");
	if (!result)
		throw "Failed to create data file!";

	// Header
	result << "TITLE = \"t=" << t << "\"" << endl;
	result << "VARIABLES = \"X\", \"Y\", \"P\", \"U\", \"V\"" << endl;
	result << "ZONE I=" << Nx << ", J=" << Ny << ", F=POINT" << endl;

	// Flowfield data
	for (int j = 1; j <= Ny; ++j)
		for (int i = 1; i <= Nx; ++i)
		{
			result << setw(WIDTH) << setprecision(DIGITS) << x[i - 1];
			result << setw(WIDTH) << setprecision(DIGITS) << y[j - 1];
			result << setw(WIDTH) << setprecision(DIGITS) << p(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << u_interp(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << v_interp(i, j);
			result << endl;
		}

	// Finalize
	result.close();
}

// Statistics at (15, 5) and i=15
void output2(int iter)
{
	static const string fn("history_at_15_5.txt");

	ofstream fout;
	if (iter == 0)
	{
		fout.open(fn, ios::out);
		if (!fout)
			throw "Failed to open history file.";

		for (int j = 0; j < Ny; ++j)
			fout << setw(WIDTH) << setprecision(DIGITS) << y[j];
		fout << endl;
	}
	else
	{
		fout.open(fn, ios::app);
		if (!fout)
			throw "Failed to open history file.";
	}

	for (int j = 1; j <= Ny; ++j)
		fout << setw(WIDTH) << setprecision(DIGITS) << u(15, j);
	fout << endl;
	for (int j = 1; j <= Ny; ++j)
		fout << setw(WIDTH) << setprecision(DIGITS) << v(15, j);
	fout << endl;
	fout << d_15_5 << endl;

	fout.close();
}

void init()
{
	cout << "mu=" << mu << endl;
	cout << "dt=" << dt << endl;

	// Init
	for (int i = 1; i < Nx; ++i)
		x[i] = L * i / (Nx - 1); // X-Coordinates
	for (int j = 1; j < Ny; ++j)
		y[j] = D * j / (Ny - 1); // Y-Coordinates

	for (int i = 1; i <= Nx + 1; ++i)
		u(i, Ny) = Ue; // U at top
	v(15, 5) = 0.5; // Initial peak to ensure 2D flow structure
}

// Solve the pressure equation.
void ImplicitMethod1()
{
	typedef Eigen::SparseMatrix<double> SpMat;
	typedef Eigen::Triplet<double> T;

	const int m = Nx * Ny;
	vector<T> coef;
	Eigen::VectorXd rhs(m);
	SpMat A(m, m);

	// Calculating coefficients
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			const int id_w = id - 1;
			const int id_e = id + 1;
			const int id_n = id + Nx;
			const int id_s = id - Nx;

			if (i == 0 || i == Nx - 1) // Inlet and Outlet
			{
				coef.emplace_back(id, id, 1.0);
				rhs(id) = 0.0;
			}
			else if (j == 0) // Bottom
			{
				coef.emplace_back(id, id, 1.0);
				coef.emplace_back(id, id_n, -1.0);
				rhs(id) = 0.0;
			}
			else if (j == Ny - 1) // Top
			{
				coef.emplace_back(id, id, 1.0);
				coef.emplace_back(id, id_s, -1.0);
				rhs(id) = 0.0;
			}
			else // Inner
			{
				// Use 0-based interface
				const double d = (rho * u_star1.at(i + 1, j) - rho * u_star1.at(i, j)) / dx + (rho * v_star1.at(i + 1, j + 1) - rho * v_star1.at(i + 1, j)) / dy;

				coef.emplace_back(id, id, a);
				coef.emplace_back(id, id_w, b);
				coef.emplace_back(id, id_e, b);
				coef.emplace_back(id, id_n, c);
				coef.emplace_back(id, id_s, c);
				rhs(id) = -d;
			}
		}

	// Construct sparse matrix
	A.setFromTriplets(coef.begin(), coef.end());

	// Solve the linear system: Ax = rhs
	Eigen::SimplicialCholesky<SpMat> chl(A);
	Eigen::VectorXd x = chl.solve(rhs);

	// Update p*
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			p_star1.at(i, j) = p.at(i, j) + x(id);
		}
}

// Solve the pressure-correction equation
void ImplicitMethod2()
{
	typedef Eigen::SparseMatrix<double> SpMat;
	typedef Eigen::Triplet<double> T;

	const int m = Nx * Ny;
	vector<T> coef;
	Eigen::VectorXd rhs(m);
	SpMat A(m, m);

	// Calculating coefficients
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			const int id_w = id - 1;
			const int id_e = id + 1;
			const int id_n = id + Nx;
			const int id_s = id - Nx;

			if (i == 0 || i == Nx - 1) // Inlet and Outlet
			{
				coef.emplace_back(id, id, 1.0);
				rhs(id) = 0.0;
			}
			else if (j == 0) // Bottom
			{
				coef.emplace_back(id, id, 1.0);
				coef.emplace_back(id, id_n, -1.0);
				rhs(id) = 0.0;
			}
			else if (j == Ny - 1) // Top
			{
				coef.emplace_back(id, id, 1.0);
				coef.emplace_back(id, id_s, -1.0);
				rhs(id) = 0.0;
			}
			else // Inner
			{
				// Use 0-based interface
				const double d = (rho*u_wedge.at(i + 1, j) - rho * u_wedge.at(i, j)) / dx + (rho * v_wedge.at(i + 1, j + 1) - rho * v_wedge.at(i + 1, j)) / dy;
				if (d > d_max)
					d_max = d;
				if (d < d_min)
					d_min = d;
				if (i == 15 && j == 5)
					d_15_5 = d;

				coef.emplace_back(id, id, a);
				coef.emplace_back(id, id_w, b);
				coef.emplace_back(id, id_e, b);
				coef.emplace_back(id, id_n, c);
				coef.emplace_back(id, id_s, c);
				rhs(id) = -d;
			}
		}

	// Construct sparse matrix
	A.setFromTriplets(coef.begin(), coef.end());

	// Solve the linear system: Ax = rhs
	Eigen::SimplicialCholesky<SpMat> chl(A);
	Eigen::VectorXd x = chl.solve(rhs);

	// Update p_prime
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			p_star2.at(i, j) = p_star1.at(i, j) + x(id);
		}
}

void PISO()
{
	/********************************************** Prediction Step ***************************************************/
	// u* at inner points
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			const double v_bar1 = 0.5*(v(i, j + 1) + v(i + 1, j + 1));
			const double v_bar2 = 0.5*(v(i, j) + v(i + 1, j));

			const double t11 = rho * pow(u(i + 1, j), 2) - rho * pow(u(i - 1, j), 2);
			const double t12 = rho * u(i, j + 1)*v_bar1 - rho * u(i, j - 1)*v_bar2;
			const double t21 = u(i + 1, j) - 2 * u(i, j) + u(i - 1, j);
			const double t22 = u(i, j + 1) - 2 * u(i, j) + u(i, j - 1);
			A(i, j) = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			const double dpdx = (p(i, j) - p(i - 1, j)) / dx;

			u_star1(i, j) = (rho * u(i, j) + A(i, j) * dt - dt * dpdx) / rho;
		}

	// u* at boundary
	for (int i = 1; i <= Nx + 1; ++i)
	{
		u_star1(i, 1) = 0.0;
		u_star1(i, Ny) = Ue;
	}
	for (int j = 2; j <= Ny - 1; ++j)
	{
		u_star1(1, j) = 2 * u_star1(2, j) - u_star1(3, j);
		u_star1(Nx + 1, j) = 2 * u_star1(Nx, j) - u_star1(Nx - 1, j);
	}

	// v* at inner points
	for (int i = 3; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			const double u_bar1 = 0.5 *(u(i, j - 1) + u(i, j));
			const double u_bar2 = 0.5 *(u(i - 1, j - 1) + u(i - 1, j));

			const double t11 = rho * v(i + 1, j) * u_bar1 - rho * v(i - 1, j) * u_bar2;
			const double t12 = rho * pow(v(i, j + 1), 2) - rho * pow(v(i, j - 1), 2);
			const double t21 = v(i + 1, j) - 2 * v(i, j) + v(i - 1, j);
			const double t22 = v(i, j + 1) - 2 * v(i, j) + v(i, j - 1);
			B(i, j) = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			const double dpdy = (p(i - 1, j) - p(i - 1, j - 1)) / dy;

			v_star1(i, j) = (rho * v(i, j) + B(i, j) * dt - dt * dpdy) / rho;
		}

	// v* at boundary
	for (int j = 2; j <= Ny; ++j)
	{
		v_star1(2, j) = 0.0;
		v_star1(1, j) = -v_star1(3, j);
		v_star1(Nx + 2, j) = 2 * v_star1(Nx + 1, j) - v_star1(Nx, j);
	}
	for (int i = 1; i <= Nx + 2; ++i)
	{
		v_star1(i, 1) = -v_star1(i, 2);
		v_star1(i, Ny + 1) = -v_star1(i, Ny);
	}

	/************************************************ Correction Step1 ************************************************/
	// Solve (p* - p)
	ImplicitMethod1();

	// u** at inner points
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			const double dpdx = (p_star1(i, j) - p_star1(i - 1, j)) / dx;
			u_star2(i, j) = (rho * u(i, j) + A(i, j) * dt - dt * dpdx) / rho;
		}

	// u** at boundary
	for (int i = 1; i <= Nx + 1; ++i)
	{
		u_star2(i, 1) = 0.0;
		u_star2(i, Ny) = Ue;
	}
	for (int j = 2; j <= Ny - 1; ++j)
	{
		u_star2(1, j) = 2 * u_star2(2, j) - u_star2(3, j);
		u_star2(Nx + 1, j) = 2 * u_star2(Nx, j) - u_star2(Nx - 1, j);
	}

	// v** at inner points
	for (int i = 3; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			const double dpdy = (p_star1(i - 1, j) - p_star1(i - 1, j - 1)) / dy;
			v_star2(i, j) = (rho * v(i, j) + B(i, j) * dt - dt * dpdy) / rho;
		}

	// v** at boundary
	for (int j = 2; j <= Ny; ++j)
	{
		v_star2(2, j) = 0.0;
		v_star2(1, j) = -v_star2(3, j);
		v_star2(Nx + 2, j) = 2 * v_star2(Nx + 1, j) - v_star2(Nx, j);
	}
	for (int i = 1; i <= Nx + 2; ++i)
	{
		v_star2(i, 1) = -v_star2(i, 2);
		v_star2(i, Ny + 1) = -v_star2(i, Ny);
	}

	/************************************************ Correction Step2 ************************************************/
	// u**^ at inner points
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			const double v_bar1 = 0.5*(v_star1(i, j + 1) + v_star1(i + 1, j + 1));
			const double v_bar2 = 0.5*(v_star1(i, j) + v_star1(i + 1, j));

			const double t11 = rho * pow(u_star1(i + 1, j), 2) - rho * pow(u_star1(i - 1, j), 2);
			const double t12 = rho * u_star1(i, j + 1)*v_bar1 - rho * u_star1(i, j - 1)*v_bar2;
			const double t21 = u_star1(i + 1, j) - 2 * u_star1(i, j) + u_star1(i - 1, j);
			const double t22 = u_star1(i, j + 1) - 2 * u_star1(i, j) + u_star1(i, j - 1);
			A_star(i, j) = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			u_wedge(i, j) = (rho * u_star2(i, j) + (A_star(i, j) - A(i, j)) * dt) / rho;
		}

	// v**^ at inner points
	for (int i = 3; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			const double u_bar1 = 0.5 *(u_star1(i, j - 1) + u_star1(i, j));
			const double u_bar2 = 0.5 *(u_star1(i - 1, j - 1) + u_star1(i - 1, j));

			const double t11 = rho * v_star1(i + 1, j) * u_bar1 - rho * v_star1(i - 1, j) * u_bar2;
			const double t12 = rho * pow(v_star1(i, j + 1), 2) - rho * pow(v_star1(i, j - 1), 2);
			const double t21 = v_star1(i + 1, j) - 2 * v_star1(i, j) + v_star1(i - 1, j);
			const double t22 = v_star1(i, j + 1) - 2 * v_star1(i, j) + v_star1(i, j - 1);
			B_star(i, j) = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			v_wedge(i, j) = (rho * v_star2(i, j) + (B_star(i, j) - B(i, j)) * dt) / rho;
		}

	d_min = numeric_limits<double>::max();
	d_max = numeric_limits<double>::min();
	ImplicitMethod2();

	/************************************************* Update u and v *************************************************/
	// Correct u at inner nodes
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			const double dpdx = (p_star2(i, j) - p_star2(i - 1, j)) / dx;
			u(i, j) = (rho * u_wedge(i, j) - dt * dpdx) / rho;
		}

	// Linear extrapolation of u at virtual nodes
	for (int j = 2; j <= Ny - 1; ++j)
	{
		u(1, j) = 2 * u(2, j) - u(3, j);
		u(Nx + 1, j) = 2 * u(Nx, j) - u(Nx - 1, j);
	}

	// Correct v at inner nodes
	for (int i = 3; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			const double dpdy = (p_star2(i - 1, j) - p_star2(i - 1, j - 1)) / dy;
			v(i, j) = (rho * v_wedge(i, j) - dt * dpdy) / rho;
		}

	// Linear extrapolation of v at right virtual nodes
	for (int j = 2; j <= Ny; ++j)
		v(Nx + 2, j) = 2 * v(Nx + 1, j) - v(Nx, j);

	// Linear extrapolation of v at both top and bottom virtual nodes
	// No-Penetration at both top and bottom
	for (int i = 1; i <= Nx + 2; ++i)
	{
		v(i, 1) = -v(i, 2);
		v(i, Ny + 1) = -v(i, Ny);
	}

	// Update p
	for (int j = 1; j <= Ny; ++j)
		for (int i = 1; i <= Nx; ++i)
			p(i, j) = p_star2(i, j);
}

bool check_convergence()
{
	// Statistics of the mass flux residue
	cout << "Max(d)=" << d_max << " Min(d)=" << d_min << endl;

	// Statistics of u
	double u_max = numeric_limits<double>::min();
	double u_min = numeric_limits<double>::max();
	for (int i = 2; i <= Nx; ++i)
		for (int j = 1; j <= Ny; ++j)
		{
			u_max = max(u_max, u(i, j));
			u_min = min(u_min, u(i, j));
		}
	cout << "Max(u)=" << u_max << " Min(u)=" << u_min << endl;

	// Statistics of v
	double v_max = numeric_limits<double>::min();
	double v_min = numeric_limits<double>::max();
	for (int i = 2; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			v_max = max(v_max, v(i, j));
			v_min = min(v_min, v(i, j));
		}
	cout << "Max(v)=" << v_max << " Min(v)=" << v_min << endl;

	// Statistics of p
	double p_max = numeric_limits<double>::min();
	double p_min = numeric_limits<double>::max();
	for (int i = 1; i <= Nx; ++i)
		for (int j = 1; j <= Ny; ++j)
		{
			p_max = max(p_max, p(i, j));
			p_min = min(p_min, p(i, j));
		}
	cout << "Max(p)=" << p_max << " Min(p)=" << p_min << endl;

	return iter_cnt > MAX_ITER_NUM || max(abs(d_max), abs(d_min)) < 1e-4;
}

void loop()
{
	bool converged = false;
	while (!converged)
	{
		++iter_cnt;
		cout << "Iter" << iter_cnt << ":" << endl;

		PISO();
		t += dt;

		output1();
		output2(iter_cnt);

		converged = check_convergence();
	}
}

int main(int argc, char *argv[])
{
	// Initialize
	init();

	// Output I.C.
	output1();
	output2(0);

	// Solve
	loop();

	return 0;
}
