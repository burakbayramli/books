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

Array2D p(Nx, Ny, Pe), p_star(Nx, Ny, Pe), p_prime(Nx, Ny, 0.0);
Array2D u(Nx + 1, Ny, 0.0), u_wedge(Nx + 1, Ny, 0.0), u_star(Nx + 1, Ny, 0.0), u_prime(Nx + 1, Ny, 0.0);
Array2D v(Nx + 2, Ny + 1, 0.0), v_wedge(Nx + 2, Ny + 1, 0.0), v_star(Nx + 2, Ny + 1, 0.0), v_prime(Nx + 2, Ny + 1, 0.0);

// Full flowfield in TECPLOT ASCII Format.
void output1(void)
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
		throw("Failed to create data file!");

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
			throw("Failed to open history file.");

		for (int j = 0; j < Ny; ++j)
			fout << setw(WIDTH) << setprecision(DIGITS) << y[j];
		fout << endl;
	}
	else
	{
		fout.open(fn, ios::app);
		if (!fout)
			throw("Failed to open history file.");
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

void init(void)
{
	cout << "mu=" << mu << endl;
	cout << "dt=" << dt << endl;

	// Init
	for (int i = 1; i < Nx; ++i)
		x[i] = L * i / (Nx - 1); // X-Coordinates
	for (int j = 1; j < Ny; ++j)
		y[j] = D * j / (Ny - 1); // Y-Coordinates

	for (int i = 1; i <= Nx + 1; ++i)
		u(i, Ny) = u_wedge(i, Ny) = u_star(i, Ny) = Ue; // U at top
	v(15, 5) = v_wedge(15, 5) = v_star(15, 5) = 0.5; // Initial peak to ensure 2D flow structure
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
				coef.push_back(T(id, id, 1.0));
				rhs(id) = Pe;
			}
			else if (j == 0) // Bottom
			{
				coef.push_back(T(id, id_n, 1.0));
				coef.push_back(T(id, id, -1.0));
				const double ddvddx = 0.0;
				//const double ddvddy = 4.0 / 3 * (v_wedge.at(i + 1, j + 2) - 3 * v_wedge.at(i + 1, j + 1)) / dydy; 
				const double ddvddy = 0.0;
				rhs(id) = mu * (ddvddx + ddvddy) * dy;
			}
			else if (j == Ny - 1) // Top
			{
				coef.push_back(T(id, id, 1.0));
				coef.push_back(T(id, id_s, -1.0));
				const double ddvddx = 0.0;
				//const double ddvddy = 4.0 / 3 * (v_wedge.at(i + 1, j - 1) - 3 * v_wedge.at(i + 1, j)) / dydy;
				const double ddvddy = 0.0;
				rhs(id) = mu * (ddvddx + ddvddy) * dy;
			}
			else // Inner
			{
				// Use 0-based interface
				const double d = (rho*u_wedge.at(i + 1, j) - rho * u_wedge.at(i, j)) / dx + (rho*v_wedge.at(i + 1, j + 1) - rho * v_wedge.at(i + 1, j)) / dy;

				coef.push_back(T(id, id, a));
				coef.push_back(T(id, id_w, b));
				coef.push_back(T(id, id_e, b));
				coef.push_back(T(id, id_n, c));
				coef.push_back(T(id, id_s, c));
				rhs(id) = -d;
			}
		}

	// Construct sparse matrix
	A.setFromTriplets(coef.begin(), coef.end());

	// Solve the linear system: Ax = rhs
	Eigen::SimplicialCholesky<SpMat> chol(A);
	Eigen::VectorXd x = chol.solve(rhs);

	// Update p
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			p.at(i, j) = x(id);
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
				coef.push_back(T(id, id, 1.0));
				rhs(id) = 0.0;
			}
			else if (j == 0) // Bottom
			{
				coef.push_back(T(id, id, 1.0));
				coef.push_back(T(id, id_n, -1.0));
				rhs(id) = 0.0;
			}
			else if (j == Ny - 1) // Top
			{
				coef.push_back(T(id, id, 1.0));
				coef.push_back(T(id, id_s, -1.0));
				rhs(id) = 0.0;
			}
			else // Inner
			{
				// Use 0-based interface
				const double d = (rho*u_star.at(i + 1, j) - rho * u_star.at(i, j)) / dx + (rho * v_star.at(i + 1, j + 1) - rho * v_star.at(i + 1, j)) / dy;
				if (d > d_max)
					d_max = d;
				if (d < d_min)
					d_min = d;
				if (i == 15 && j == 5)
					d_15_5 = d;

				coef.push_back(T(id, id, a));
				coef.push_back(T(id, id_w, b));
				coef.push_back(T(id, id_e, b));
				coef.push_back(T(id, id_n, c));
				coef.push_back(T(id, id_s, c));
				rhs(id) = -d;
			}
		}

	// Construct sparse matrix
	A.setFromTriplets(coef.begin(), coef.end());

	// Solve the linear system: Ax = rhs
	Eigen::SimplicialCholesky<SpMat> chol(A);
	Eigen::VectorXd x = chol.solve(rhs);

	// Update p_prime
	for (int i = 0; i < Nx; ++i)
		for (int j = 0; j < Ny; ++j)
		{
			const int id = j * Nx + i;
			p_prime.at(i, j) = x(id);
		}
}

void SIMPLER(void)
{
	// u_wedge at inner points
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			double v_bar1 = 0.5*(v(i, j + 1) + v(i + 1, j + 1));
			double v_bar2 = 0.5*(v(i, j) + v(i + 1, j));

			double t11 = rho * pow(u(i + 1, j), 2) - rho * pow(u(i - 1, j), 2);
			double t12 = rho * u(i, j + 1)*v_bar1 - rho * u(i, j - 1)*v_bar2;
			double t21 = u(i + 1, j) - 2 * u(i, j) + u(i - 1, j);
			double t22 = u(i, j + 1) - 2 * u(i, j) + u(i, j - 1);
			double A = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			u_wedge(i, j) = (rho * u(i, j) + A * dt) / rho;
		}

	// v_wedge at inner points
	for (int i = 3; i <= Nx; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			double u_bar1 = 0.5 *(u(i, j - 1) + u(i, j));
			double u_bar2 = 0.5 *(u(i - 1, j - 1) + u(i - 1, j));

			double t11 = rho * v(i + 1, j) * u_bar1 - rho * v(i - 1, j) * u_bar2;
			double t12 = rho * pow(v(i, j + 1), 2) - rho * pow(v(i, j - 1), 2);
			double t21 = v(i + 1, j) - 2 * v(i, j) + v(i - 1, j);
			double t22 = v(i, j + 1) - 2 * v(i, j) + v(i, j - 1);
			double B = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			v_wedge(i, j) = (rho * v(i, j) + B * dt) / rho;
		}

	// Solve p
	ImplicitMethod1();

	// Set p_star to p
	for (int j = 1; j <= Ny; ++j)
		for (int i = 1; i <= Nx; ++i)
			p_star(i, j) = p(i, j);

	// u_star at inner points
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			double v_bar1 = 0.5*(v(i, j + 1) + v(i + 1, j + 1));
			double v_bar2 = 0.5*(v(i, j) + v(i + 1, j));

			double t11 = rho * pow(u(i + 1, j), 2) - rho * pow(u(i - 1, j), 2);
			double t12 = rho * u(i, j + 1)*v_bar1 - rho * u(i, j - 1)*v_bar2;
			double t21 = u(i + 1, j) - 2 * u(i, j) + u(i - 1, j);
			double t22 = u(i, j + 1) - 2 * u(i, j) + u(i, j - 1);
			double A = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			u_star(i, j) = (rho * u(i, j) + A * dt - dt / dx * (p_star(i, j) - p_star(i - 1, j))) / rho;
		}

	// v_star at inner points
	for (int i = 3; i <= Nx + 1; ++i)
		for (int j = 2; j <= Ny; ++j)
		{
			double u_bar1 = 0.5 *(u(i, j - 1) + u(i, j));
			double u_bar2 = 0.5 *(u(i - 1, j - 1) + u(i - 1, j));

			double t11 = rho * v(i + 1, j) * u_bar1 - rho * v(i - 1, j) * u_bar2;
			double t12 = rho * pow(v(i, j + 1), 2) - rho * pow(v(i, j - 1), 2);
			double t21 = v(i + 1, j) - 2 * v(i, j) + v(i - 1, j);
			double t22 = v(i, j + 1) - 2 * v(i, j) + v(i, j - 1);
			double B = -(t11 / dx2 + t12 / dy2) + mu * (t21 / dxdx + t22 / dydy);

			v_star(i, j) = (rho * v(i, j) + B * dt - dy / dy * (p_star(i - 1, j) - p_star(i - 1, j - 1))) / rho;
		}

	d_min = numeric_limits<double>::max();
	d_max = numeric_limits<double>::min();
	ImplicitMethod2();

	// Correct u at inner nodes
	for (int j = 2; j <= Ny - 1; ++j)
		for (int i = 2; i <= Nx; ++i)
		{
			u_prime(i, j) = -dt / dx * (p_prime(i, j) - p_prime(i - 1, j)) / rho; // u_prime
			u(i, j) = u_star(i, j) + u_prime(i, j);
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
			v_prime(i, j) = -dt / dy * (p_prime(i - 1, j) - p_prime(i - 1, j - 1)) / rho; // v_prime
			v(i, j) = v_star(i, j) + v_prime(i, j);
		}

	// Linear extrapolation of v at both top and bottom virtual nodes
	// No-Penetration at both top and bottom
	for (int i = 2; i <= Nx + 1; ++i)
	{
		v(i, 1) = -v(i, 2);
		v(i, Ny + 1) = -v(i, Ny);
	}

	// Linear extrapolation of v at right virtual nodes
	for (int j = 2; j <= Ny; ++j)
		v(Nx + 2, j) = 2 * v(Nx + 1, j) - v(Nx, j);
}

bool check_convergence(void)
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

void loop(void)
{
	bool converged = false;
	while (!converged)
	{
		++iter_cnt;
		cout << "Iter" << iter_cnt << ":" << endl;

		SIMPLER();
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
