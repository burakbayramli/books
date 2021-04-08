#include <iostream>
#include <fstream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <algorithm>
#include <cassert>
#include <limits>
#include <string>
#include <stdexcept>

using namespace std;

class Array2D
{
private:
	vector<double> m_data;
	size_t m_Nx, m_Ny;

public:
	Array2D(size_t nx, size_t ny, double val = 0.0) :
		m_data(nx * ny, val)
	{
		m_Nx = nx;
		m_Ny = ny;
	}

	~Array2D() = default;

	// 0-based indexing
	double &at(size_t i, size_t j)
	{
		size_t idx = i + m_Nx * j;
		return m_data[idx];
	}

	double at(size_t i, size_t j) const
	{
		size_t idx = i + m_Nx * j;
		return m_data[idx];
	}

	// 1-based indexing
	double &operator()(size_t i, size_t j)
	{
		return at(i - 1, j - 1);
	}

	double operator()(size_t i, size_t j) const
	{
		return at(i - 1, j - 1);
	}
};

const double LHORI = 1e-5; // m
const double Re = 1000.0;
const double Pr = 0.71;
const double a = 340.28; // m/s
const double Ma = 4.0;
const double R = 287.0; // J/(Kg*K)
const double G0 = 1.4;
const double Cv = R / (G0 - 1); // J/(Kg*K)
const double Cp = G0 * Cv; // J/(Kg*K)

// Grid params
const size_t IMIN = 1, IMAX = 70;
const size_t JMIN = 1, JMAX = 70;
const double dx = LHORI / (IMAX - 1);
const double dx2 = 2.0 * dx, dxdx = pow(dx, 2);
const double delta = 5 * LHORI / std::sqrt(Re);
const double LVERT = 5 * delta;
const double dy = LVERT / (JMAX - 1);
const double dy2 = 2.0 * dy, dydy = pow(dy, 2);
vector<double> x(IMAX, 0.0), y(JMAX, 0.0);

double dt = 1e-5;
double t = 0.0;

size_t iter = 0;
const size_t MAX_ITER = 10000;

const double u_inf = Ma * a;
const double v_inf = 0.0;
const double p_inf = 101325.0;
const double T_inf = 288.16;

// Primitive variables
Array2D rho(IMAX, JMAX, 0.0);
Array2D u(IMAX, JMAX, 0.0);
Array2D v(IMAX, JMAX, 0.0);
Array2D p(IMAX, JMAX, 0.0);
Array2D T(IMAX, JMAX, T_inf);
Array2D e(IMAX, JMAX, 0.0); // Internal energy per unit mass

// Physical properties
Array2D mu(IMAX, JMAX, 0.0); // Kg/(m*s)
Array2D k(IMAX, JMAX, 0.0); // J/(s*m*K)
Array2D lambda(IMAX, JMAX, 0.0); // Kg/(m*s)

// Conservative variables
Array2D U1(IMAX, JMAX, 0.0); // rho
Array2D U2(IMAX, JMAX, 0.0); // rho u
Array2D U3(IMAX, JMAX, 0.0); // rho v
Array2D U5(IMAX, JMAX, 0.0); // rho(e+V^2 / 2)

Array2D E1(IMAX, JMAX, 0.0);
Array2D E2(IMAX, JMAX, 0.0);
Array2D E3(IMAX, JMAX, 0.0);
Array2D E5(IMAX, JMAX, 0.0);

Array2D F1(IMAX, JMAX, 0.0);
Array2D F2(IMAX, JMAX, 0.0);
Array2D F3(IMAX, JMAX, 0.0);
Array2D F5(IMAX, JMAX, 0.0);

Array2D dU1dt(IMAX, JMAX, 0.0);
Array2D dU2dt(IMAX, JMAX, 0.0);
Array2D dU3dt(IMAX, JMAX, 0.0);
Array2D dU5dt(IMAX, JMAX, 0.0);

// Predicted values
Array2D rho_bar(IMAX, JMAX, 0.0);
Array2D u_bar(IMAX, JMAX, 0.0);
Array2D v_bar(IMAX, JMAX, 0.0);
Array2D p_bar(IMAX, JMAX, 0.0);
Array2D T_bar(IMAX, JMAX, T_inf);
Array2D e_bar(IMAX, JMAX, 0.0);

Array2D mu_bar(IMAX, JMAX, 0.0);
Array2D k_bar(IMAX, JMAX, 0.0);
Array2D lambda_bar(IMAX, JMAX, 0.0);

Array2D U1_bar(IMAX, JMAX, 0.0);
Array2D U2_bar(IMAX, JMAX, 0.0);
Array2D U3_bar(IMAX, JMAX, 0.0);
Array2D U5_bar(IMAX, JMAX, 0.0);

Array2D E1_bar(IMAX, JMAX, 0.0);
Array2D E2_bar(IMAX, JMAX, 0.0);
Array2D E3_bar(IMAX, JMAX, 0.0);
Array2D E5_bar(IMAX, JMAX, 0.0);

Array2D F1_bar(IMAX, JMAX, 0.0);
Array2D F2_bar(IMAX, JMAX, 0.0);
Array2D F3_bar(IMAX, JMAX, 0.0);
Array2D F5_bar(IMAX, JMAX, 0.0);

Array2D dU1dt_bar(IMAX, JMAX, 0.0);
Array2D dU2dt_bar(IMAX, JMAX, 0.0);
Array2D dU3dt_bar(IMAX, JMAX, 0.0);
Array2D dU5dt_bar(IMAX, JMAX, 0.0);

// Averaged temporal derivatives
Array2D dU1dt_av(IMAX, JMAX, 0.0);
Array2D dU2dt_av(IMAX, JMAX, 0.0);
Array2D dU3dt_av(IMAX, JMAX, 0.0);
Array2D dU5dt_av(IMAX, JMAX, 0.0);

inline double Sutherland(double T)
{
	static const double mu0 = 1.7894e-5; // Kg/(m*s)
	static const double T0 = 288.16; // K

	return mu0 * pow(T / T0, 1.5) * (T0 + 110.0) / (T + 110.0);
}

double TimeStep()
{
	static const double CFL = 0.5;
	static const double nu_factor = max(4.0 / 3, G0 / Pr);

	double ret = numeric_limits<double>::max();

	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			const double loc_nu = mu(i, j) / rho(i, j) * nu_factor;
			const double metric0 = 1.0 / dxdx + 1.0 / dydy;
			const double metric1 = sqrt(metric0);
			const double loc_C = sqrt(G0 * R * T(i, j));
			const double loc_dt = 1.0 / (abs(u(i, j)) / dx + abs(v(i, j)) / dy + loc_C * metric1 + 2 * loc_nu * metric0);
			ret = min(ret, loc_dt);
		}

	return CFL * ret;
}

inline bool at_boundary(size_t i, size_t j)
{
	return i == IMIN || i == IMAX || j == JMIN || j == JMAX;
}

void Adiabatic_BC(Array2D &rho0, Array2D &u0, Array2D &v0, Array2D &p0, Array2D &T0, Array2D &e0)
{
	// Front tip
	u0(IMIN, JMIN) = 0.0;
	v0(IMIN, JMIN) = 0.0;
	p0(IMIN, JMIN) = p_inf;
	T0(IMIN, JMIN) = T_inf;

	// Inlet
	for (size_t j = JMIN + 1; j <= JMAX; ++j)
	{
		u0(IMIN, j) = u_inf;
		v0(IMIN, j) = 0.0;
		p0(IMIN, j) = p_inf;
		T0(IMIN, j) = T_inf;
	}

	// Top(Far)
	for (size_t i = IMIN + 1; i <= IMAX; ++i)
	{
		u0(i, JMAX) = u_inf;
		v0(i, JMAX) = 0.0;
		p0(i, JMAX) = p_inf;
		T0(i, JMAX) = T_inf;
	}

	// Bottom
	for (size_t i = IMIN + 1; i <= IMAX; ++i)
	{
		u0(i, JMIN) = 0.0;
		v0(i, JMIN) = 0.0;
		p0(i, JMIN) = 2 * p0(i, JMIN + 1) - p0(i, JMIN + 2);
		T0(i, JMIN) = T0(i, JMIN + 1); // !!!
	}

	// Outlet
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
	{
		u0(IMAX, j) = 2 * u0(IMAX - 1, j) - u0(IMAX - 2, j);
		v0(IMAX, j) = 2 * v0(IMAX - 1, j) - v0(IMAX - 2, j);
		p0(IMAX, j) = 2 * p0(IMAX - 1, j) - p0(IMAX - 2, j);
		T0(IMAX, j) = 2 * T0(IMAX - 1, j) - T0(IMAX - 2, j);
	}

	// Derived Variables
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
			if (at_boundary(i, j))
			{
				rho0(i, j) = p0(i, j) / (R * T0(i, j));
				e0(i, j) = Cv * T0(i, j);
			}
}

void update_physical_properties(const Array2D &T0, Array2D &mu0, Array2D &k0, Array2D &lambda0)
{
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			mu0(i, j) = Sutherland(T0(i, j));
			k0(i, j) = mu0(i, j) * Cp / Pr;
			lambda0(i, j) = -2.0 / 3 * mu0(i, j); // Follow Stokes's hypothesis
		}
}

void init()
{
	/********************************** Grid **********************************/
	cout << "Lx=" << LHORI << "m, dx=" << dx << "m" << endl;
	cout << "Ly=" << LVERT << "m, dy=" << dy << "m" << endl;
	for (size_t i = 1; i < IMAX; ++i)
		x[i] = x[i - 1] + dx;

	for (size_t j = 1; j < JMAX; ++j)
		y[j] = y[j - 1] + dy;

	/********************************** I.C. **********************************/
	// Inner
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			u(i, j) = u_inf;
			v(i, j) = v_inf;
			p(i, j) = p_inf;
			T(i, j) = T_inf;
			rho(i, j) = p(i, j) / (R * T(i, j));
			e(i, j) = Cv * T(i, j);
		}

	/********************************** B.C. **********************************/
	Adiabatic_BC(rho, u, v, p, T, e);

	/************************ Conservative Variables **************************/
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			U1(i, j) = rho(i, j);
			U2(i, j) = rho(i, j) * u(i, j);
			U3(i, j) = rho(i, j) * v(i, j);
			const double K = 0.5 * (pow(u(i, j), 2) + pow(v(i, j), 2));
			U5(i, j) = rho(i, j) * (e(i, j) + K);
		}

	/************************** Physical Properties ***************************/
	update_physical_properties(T, mu, k, lambda);
}

void MacCormack()
{
	/***************************** Forward Difference *************************/
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			// Backward difference within E for x-derivatives
			double dudx = 0.0, dvdx = 0.0, dTdx = 0.0;
			if (i == IMIN)
			{
				dudx = (u(i + 1, j) - u(i, j)) / dx;
				dvdx = (v(i + 1, j) - v(i, j)) / dx;
				dTdx = (T(i + 1, j) - T(i, j)) / dx;
			}
			else
			{
				dudx = (u(i, j) - u(i - 1, j)) / dx;
				dvdx = (v(i, j) - v(i - 1, j)) / dx;
				dTdx = (T(i, j) - T(i - 1, j)) / dx;
			}

			// Central difference within E for y-derivatives
			double dudy = 0.0, dvdy = 0.0;
			if (j == JMIN)
			{
				dudy = (u(i, j + 2) - 4 * u(i, j + 1) + 3 * u(i, j)) / (-dy2);
				dvdy = (v(i, j + 1) - v(i, j)) / dy;
			}
			else if (j == JMAX)
			{
				dudy = (u(i, j) - u(i, j - 1)) / dy;
				dvdy = (v(i, j) - v(i, j - 1)) / dy;
			}
			else
			{
				dudy = (u(i, j + 1) - u(i, j - 1)) / dy2;
				dvdy = (v(i, j + 1) - v(i, j - 1)) / dy2;
			}

			// Shear stress
			const double divergence = dudx + dvdy;
			const double tau_xx = lambda(i, j) * divergence + 2 * mu(i, j) * dudx;
			const double tau_xy = mu(i, j) * (dudy + dvdx);

			// Heat conduction
			const double q_x = -k(i, j) * dTdx;

			// Elements of E
			E1(i, j) = rho(i, j) * u(i, j);
			E2(i, j) = rho(i, j) * pow(u(i, j), 2) + p(i, j) - tau_xx;
			E3(i, j) = rho(i, j) * u(i, j) * v(i, j) - tau_xy;
			E5(i, j) = (U5(i, j) + p(i, j)) * u(i, j) - u(i, j) * tau_xx - v(i, j) * tau_xy + q_x;
		}

	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			// Central difference within F for x-derivatives
			double dudx = 0.0, dvdx = 0.0;
			if (i == IMIN)
			{
				dudx = (u(i + 1, j) - u(i, j)) / dx;
				dvdx = (v(i + 1, j) - v(i, j)) / dx;
			}
			else if (i == IMAX)
			{
				dudx = (u(i, j) - u(i - 1, j)) / dx;
				dvdx = (v(i, j) - v(i - 1, j)) / dx;
			}
			else
			{
				dudx = (u(i + 1, j) - u(i - 1, j)) / dx2;
				dvdx = (v(i + 1, j) - v(i - 1, j)) / dx2;
			}

			// Backward difference within F for y-derivatives
			double dudy = 0.0, dvdy = 0.0, dTdy = 0.0;
			if (j == JMIN)
			{
				dudy = (u(i, j + 2) - 4 * u(i, j + 1) + 3 * u(i, j)) / (-dy2);
				dvdy = (v(i, j + 1) - v(i, j)) / dy;
				dTdy = (T(i, j + 1) - T(i, j)) / dy;
			}
			else
			{
				dudy = (u(i, j) - u(i, j - 1)) / dy;
				dvdy = (v(i, j) - v(i, j - 1)) / dy;
				dTdy = (T(i, j) - T(i, j - 1)) / dy;
			}

			// Shear stress
			const double divergence = dudx + dvdy;
			const double tau_xy = mu(i, j) * (dudy + dvdx);
			const double tau_yy = lambda(i, j) * divergence + 2 * mu(i, j) * dvdy;

			// Heat conduction
			const double q_y = -k(i, j) * dTdy;

			// Elements of F
			F1(i, j) = rho(i, j) * v(i, j);
			F2(i, j) = rho(i, j) * u(i, j) * v(i, j) - tau_xy;
			F3(i, j) = rho(i, j) * pow(v(i, j), 2) + p(i, j) - tau_yy;
			F5(i, j) = (U5(i, j) + p(i, j)) * v(i, j) - u(i, j) * tau_xy - v(i, j) * tau_yy + q_y;
		}

	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			const double dE1dx = (E1(i + 1, j) - E1(i, j)) / dx;
			const double dF1dy = (F1(i, j + 1) - F1(i, j)) / dy;
			dU1dt(i, j) = -(dE1dx + dF1dy);

			const double dE2dx = (E2(i + 1, j) - E2(i, j)) / dx;
			const double dF2dy = (F2(i, j + 1) - F2(i, j)) / dy;
			dU2dt(i, j) = -(dE2dx + dF2dy);

			const double dE3dx = (E3(i + 1, j) - E3(i, j)) / dx;
			const double dF3dy = (F3(i, j + 1) - F3(i, j)) / dy;
			dU3dt(i, j) = -(dE3dx + dF3dy);

			const double dE5dx = (E5(i + 1, j) - E5(i, j)) / dx;
			const double dF5dy = (F5(i, j + 1) - F5(i, j)) / dy;
			dU5dt(i, j) = -(dE5dx + dF5dy);
		}

	/******************************* Prediction *******************************/
	// Conservative values at inner
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			U1_bar(i, j) = U1(i, j) + dU1dt(i, j) * dt;
			U2_bar(i, j) = U2(i, j) + dU2dt(i, j) * dt;
			U3_bar(i, j) = U3(i, j) + dU3dt(i, j) * dt;
			U5_bar(i, j) = U5(i, j) + dU5dt(i, j) * dt;
		}

	// Primitive values at inner
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			rho_bar(i, j) = U1_bar(i, j);
			u_bar(i, j) = U2_bar(i, j) / U1_bar(i, j);
			v_bar(i, j) = U3_bar(i, j) / U1_bar(i, j);
			const double K_bar = 0.5 * (pow(u_bar(i, j), 2) + pow(v_bar(i, j), 2));
			e_bar(i, j) = U5_bar(i, j) / U1_bar(i, j) - K_bar;
			T_bar(i, j) = e_bar(i, j) / Cv;
			p_bar(i, j) = rho_bar(i, j) * R * T_bar(i, j);
		}

	// Primitive values at boundary
	Adiabatic_BC(rho_bar, u_bar, v_bar, p_bar, T_bar, e_bar);

	// Conservative values at boundary
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
			if (at_boundary(i, j))
			{
				U1_bar(i, j) = rho_bar(i, j);
				U2_bar(i, j) = rho_bar(i, j) * u_bar(i, j);
				U3_bar(i, j) = rho_bar(i, j) * v_bar(i, j);
				const double K = 0.5 * (pow(u_bar(i, j), 2) + pow(v_bar(i, j), 2));
				U5_bar(i, j) = rho_bar(i, j) * (e_bar(i, j) + K);
			}

	/********************************* Checking *******************************/
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
			if (rho_bar(i, j) < 0 || T_bar(i, j) < 0 || p_bar(i, j) < 0)
			{
				string msg("\n(" + to_string(i) + ", " + to_string(j) + "):");
				msg += string(" rho_bar=" + to_string(rho_bar(i, j)));
				msg += string(" T_bar=" + to_string(T_bar(i, j)));
				msg += string(" p_bar=" + to_string(p_bar(i, j)));
				throw runtime_error(msg);
			}

	/*************************** Backward Difference **************************/
	update_physical_properties(T_bar, mu_bar, k_bar, lambda_bar);

	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			// Forward difference within E for x-derivatives
			double dudx = 0.0, dvdx = 0.0, dTdx = 0.0;
			if (i == IMAX)
			{
				dudx = (u_bar(i, j) - u_bar(i - 1, j)) / dx;
				dvdx = (v_bar(i, j) - v_bar(i - 1, j)) / dx;
				dTdx = (T_bar(i, j) - T_bar(i - 1, j)) / dx;
			}
			else
			{
				dudx = (u_bar(i + 1, j) - u_bar(i, j)) / dx;
				dvdx = (v_bar(i + 1, j) - v_bar(i, j)) / dx;
				dTdx = (T_bar(i + 1, j) - T_bar(i, j)) / dx;
			}

			// Central difference within E for y-derivatives
			double dudy = 0.0, dvdy = 0.0;
			if (j == JMIN)
			{
				dudy = (u_bar(i, j + 2) - 4 * u_bar(i, j + 1) + 3 * u_bar(i, j)) / (-dy2);
				dvdy = (v_bar(i, j + 1) - v_bar(i, j)) / dy;
			}
			else if (j == JMAX)
			{
				dudy = (u_bar(i, j) - u_bar(i, j - 1)) / dy;
				dvdy = (v_bar(i, j) - v_bar(i, j - 1)) / dy;
			}
			else
			{
				dudy = (u_bar(i, j + 1) - u_bar(i, j - 1)) / dy2;
				dvdy = (v_bar(i, j + 1) - v_bar(i, j - 1)) / dy2;
			}

			// Shear stress
			const double divergence = dudx + dvdy;
			const double tau_xx = lambda_bar(i, j) * divergence + 2 * mu_bar(i, j) * dudx;
			const double tau_xy = mu_bar(i, j) * (dudy + dvdx);

			// Heat conduction
			const double q_x = -k_bar(i, j) * dTdx;

			// Elements of E
			E1_bar(i, j) = rho_bar(i, j) * u_bar(i, j);
			E2_bar(i, j) = rho_bar(i, j) * pow(u_bar(i, j), 2) + p_bar(i, j) - tau_xx;
			E3_bar(i, j) = rho_bar(i, j) * u_bar(i, j) * v_bar(i, j) - tau_xy;
			E5_bar(i, j) = (U5_bar(i, j) + p_bar(i, j)) * u_bar(i, j) - u_bar(i, j) * tau_xx - v_bar(i, j) * tau_xy + q_x;
		}

	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
		{
			// Central difference within F for x-derivatives
			double dudx = 0.0, dvdx = 0.0;
			if (i == IMIN)
			{
				dudx = (u_bar(i + 1, j) - u_bar(i, j)) / dx;
				dvdx = (v_bar(i + 1, j) - v_bar(i, j)) / dx;
			}
			else if (i == IMAX)
			{
				dudx = (u_bar(i, j) - u_bar(i - 1, j)) / dx;
				dvdx = (v_bar(i, j) - v_bar(i - 1, j)) / dx;
			}
			else
			{
				dudx = (u_bar(i + 1, j) - u_bar(i - 1, j)) / dx2;
				dvdx = (v_bar(i + 1, j) - v_bar(i - 1, j)) / dx2;
			}

			// Forward difference within F for y-derivatives
			double dudy = 0.0, dvdy = 0.0, dTdy = 0.0;
			if (j == JMAX)
			{
				dudy = (u_bar(i, j) - u_bar(i, j - 1)) / dy;
				dvdy = (v_bar(i, j) - v_bar(i, j - 1)) / dy;
				dTdy = (T_bar(i, j) - T_bar(i, j - 1)) / dy;
			}
			else
			{
				dudy = (u_bar(i, j + 1) - u_bar(i, j)) / dy;
				dvdy = (v_bar(i, j + 1) - v_bar(i, j)) / dy;
				dTdy = (T_bar(i, j + 1) - T_bar(i, j)) / dy;
			}

			// Shear stress
			const double divergence = dudx + dvdy;
			const double tau_xy = mu_bar(i, j) * (dudy + dvdx);
			const double tau_yy = lambda_bar(i, j) * divergence + 2 * mu_bar(i, j) * dvdy;

			// Heat conduction
			const double q_y = -k_bar(i, j) * dTdy;

			// Elements of F
			F1_bar(i, j) = rho_bar(i, j) * v_bar(i, j);
			F2_bar(i, j) = rho_bar(i, j) * u_bar(i, j) * v_bar(i, j) - tau_xy;
			F3_bar(i, j) = rho_bar(i, j) * pow(v_bar(i, j), 2) + p_bar(i, j) - tau_yy;
			F5_bar(i, j) = (U5_bar(i, j) + p_bar(i, j)) * v_bar(i, j) - u_bar(i, j) * tau_xy - v_bar(i, j) * tau_yy + q_y;
		}

	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			const double dE1dx = (E1_bar(i, j) - E1_bar(i - 1, j)) / dx;
			const double dF1dy = (F1_bar(i, j) - F1_bar(i, j - 1)) / dy;
			dU1dt_bar(i, j) = -(dE1dx + dF1dy);

			const double dE2dx = (E2_bar(i, j) - E2_bar(i - 1, j)) / dx;
			const double dF2dy = (F2_bar(i, j) - F2_bar(i, j - 1)) / dy;
			dU2dt_bar(i, j) = -(dE2dx + dF2dy);

			const double dE3dx = (E3_bar(i, j) - E3_bar(i - 1, j)) / dx;
			const double dF3dy = (F3_bar(i, j) - F3_bar(i, j - 1)) / dy;
			dU3dt_bar(i, j) = -(dE3dx + dF3dy);

			const double dE5dx = (E5_bar(i, j) - E5_bar(i - 1, j)) / dx;
			const double dF5dy = (F5_bar(i, j) - F5_bar(i, j - 1)) / dy;
			dU5dt_bar(i, j) = -(dE5dx + dF5dy);
		}

	/********************************* Average ********************************/
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			dU1dt_av(i, j) = 0.5 * (dU1dt(i, j) + dU1dt_bar(i, j));
			dU2dt_av(i, j) = 0.5 * (dU2dt(i, j) + dU2dt_bar(i, j));
			dU3dt_av(i, j) = 0.5 * (dU3dt(i, j) + dU3dt_bar(i, j));
			dU5dt_av(i, j) = 0.5 * (dU5dt(i, j) + dU5dt_bar(i, j));
		}

	/********************************* Update *********************************/
	// Inner values
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			// Conservative values
			U1(i, j) += dU1dt_av(i, j) * dt;
			U2(i, j) += dU2dt_av(i, j) * dt;
			U3(i, j) += dU3dt_av(i, j) * dt;
			U5(i, j) += dU5dt_av(i, j) * dt;

			// Primitive values
			rho(i, j) = U1(i, j);
			u(i, j) = U2(i, j) / U1(i, j);
			v(i, j) = U3(i, j) / U1(i, j);
			const double K = 0.5 * (pow(u(i, j), 2) + pow(v(i, j), 2));
			e(i, j) = U5(i, j) / U1(i, j) - K;
			T(i, j) = e(i, j) / Cv;
			p(i, j) = rho(i, j) * R * T(i, j);
		}

	// Primitive values at boundary
	Adiabatic_BC(rho, u, v, p, T, e);

	// Conservative values at boundary
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
			if (at_boundary(i, j))
			{
				U1(i, j) = rho(i, j);
				U2(i, j) = rho(i, j) * u(i, j);
				U3(i, j) = rho(i, j) * v(i, j);
				const double K = 0.5 * (pow(u(i, j), 2) + pow(v(i, j), 2));
				U5(i, j) = rho(i, j) * (e(i, j) + K);
			}

	/********************************* Checking *******************************/
	for (size_t j = JMIN; j <= JMAX; ++j)
		for (size_t i = IMIN; i <= IMAX; ++i)
			if (rho(i, j) < 0 || T(i, j) < 0 || p(i, j) < 0)
			{
				string msg("\n(" + to_string(i) + ", " + to_string(j) + "):");
				msg += string(" rho=" + to_string(rho(i, j)));
				msg += string(" T=" + to_string(T(i, j)));
				msg += string(" p=" + to_string(p(i, j)));
				throw runtime_error(msg);
			}

	// Physical properties
	update_physical_properties(T, mu, k, lambda);
}

void write_tecplot(size_t n)
{
	// Output format params
	static const size_t WIDTH = 16;
	static const size_t DIGITS = 7;

	// Create Tecplot data file.
	ofstream result("flow" + to_string(n) + ".dat");
	if (!result)
		throw runtime_error("Failed to create data file!");

	// Header
	result << R"(TITLE = "Flowfield at t=)" << t << R"(s")" << endl;
	result << R"(VARIABLES = "X", "Y", "rho", "U", "V", "P", "T", "e", "U1", "U2", "U3", "U5")" << endl;
	result << "ZONE I=" << IMAX << ", J=" << JMAX << ", F=POINT" << endl;

	// Flow-field data
	for (int j = 1; j <= JMAX; ++j)
		for (int i = 1; i <= IMAX; ++i)
		{
			result << setw(WIDTH) << setprecision(DIGITS) << x[i - 1];
			result << setw(WIDTH) << setprecision(DIGITS) << y[j - 1];
			result << setw(WIDTH) << setprecision(DIGITS) << rho(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << u(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << v(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << p(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << T(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << e(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << U1(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << U2(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << U3(i, j);
			result << setw(WIDTH) << setprecision(DIGITS) << U5(i, j);
			result << endl;
		}

	// Finalize
	result.close();
}

void write_user(size_t n)
{
	// Output format params
	static const size_t WIDTH = 16;
	static const size_t DIGITS = 7;

	// Output file name
	static const string fn("history.txt");

	ofstream fout;
	if (n == 0)
	{
		fout.open(fn, ios::out);
		if (!fout)
			throw runtime_error("Failed to open history file.");

		fout << u_inf << "\t" << v_inf << "\t" << p_inf << "\t" << T_inf << endl;

		for (size_t i = 0; i < IMAX; ++i)
			fout << setw(WIDTH) << setprecision(DIGITS) << x[i];
		fout << endl;
		for (size_t j = 0; j < JMAX; ++j)
			fout << setw(WIDTH) << setprecision(DIGITS) << y[j];
		fout << endl;
	}
	else
	{
		fout.open(fn, ios::app);
		if (!fout)
			throw runtime_error("Failed to open history file.");
	}

	// Pressure at plate surface
	for (size_t i = IMIN; i <= IMAX; ++i)
		fout << setw(WIDTH) << setprecision(DIGITS) << p(i, 1);
	fout << endl;

	// Pressure at outlet
	for (size_t j = JMIN; j <= JMAX; ++j)
		fout << setw(WIDTH) << setprecision(DIGITS) << p(IMAX, j);
	fout << endl;

	// Temperature at outlet
	for (size_t j = JMIN; j <= JMAX; ++j)
		fout << setw(WIDTH) << setprecision(DIGITS) << T(IMAX, j);
	fout << endl;

	// Stream-wise velocity at outlet
	for (size_t j = JMIN; j <= JMAX; ++j)
		fout << setw(WIDTH) << setprecision(DIGITS) << u(IMAX, j);
	fout << endl;

	// Mach number at outlet
	for (size_t j = JMIN; j <= JMAX; ++j)
	{
		const double loc_C = sqrt(G0 * R * T(IMAX, j));
		const double loc_V = sqrt(pow(u(IMAX, j), 2) + pow(v(IMAX, j), 2));
		const double loc_Ma = loc_V / loc_C;
		fout << setw(WIDTH) << setprecision(DIGITS) << loc_Ma;
	}
	fout << endl;

	fout.close();
}

void output()
{
	if (!(iter % 100))
		write_tecplot(iter);

	write_user(iter);
}

bool check_convergence()
{
	double drho_max = 0.0;
	size_t mI = 0, mJ = 0;
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
		for (size_t i = IMIN + 1; i <= IMAX - 1; ++i)
		{
			double loc_drho = abs(dU1dt_av(i, j)) * dt;
			if (loc_drho > drho_max)
			{
				drho_max = loc_drho;
				mI = i;
				mJ = j;
			}
		}

	cout << "\tMAX(drho)=" << drho_max << "Kg/m^3, at(" << mI << ", " << mJ << ")" << endl;

	return drho_max < 1e-8 || iter > MAX_ITER;
}

// Explicit Time-Marching
void solve()
{
	bool converged = false;
	while (!converged)
	{
		++iter;
		cout << "Iter" << iter << ":" << endl;

		dt = TimeStep();
		if (dt < 0)
			throw runtime_error("dt=" + to_string(dt) + "s");
		cout << "\tt=" << t << "s, dt=" << dt << "s" << endl;

		MacCormack();

		t += dt;
		output();
		converged = check_convergence();
	}
	cout << "Converged!" << endl;
	write_tecplot(iter);
}

void check_mdot()
{
	double in = 0.0, out = 0.0;

	in += rho(IMIN, JMIN) * u(IMIN, JMIN) * dy / 2;
	out += rho(IMAX, JMIN) * u(IMAX, JMIN) * dy / 2;
	for (size_t j = JMIN + 1; j <= JMAX - 1; ++j)
	{
		in += rho(IMIN, j) * u(IMIN, j) * dy;
		out += rho(IMAX, j) * u(IMAX, j) * dy;
	}
	in += rho(IMIN, JMAX) * u(IMIN, JMAX) * dy / 2;
	out += rho(IMAX, JMAX) * u(IMAX, JMAX) * dy / 2;

	cout << "mdot at inlet: " << in << " Kg/(m*s)" << endl;
	cout << "mdot at outlet: " << out << " Kg/(m*s)" << endl;
	cout << "Relative error: " << abs((out - in) / in) * 100 << "%" << endl;
}

int main(int argc, char *argv[])
{
	init();
	output();
	solve();
	check_mdot();
	return 0;
}
