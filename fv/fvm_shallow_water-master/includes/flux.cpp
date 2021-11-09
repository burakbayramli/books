#include "sw.h"

// Entropy fix
double entropy_fix(double lambda)
{
	double epsilon = 2.0;
	if (abs(lambda) >= epsilon)
	{
		return abs(lambda);
	}
	else
	{
		return (lambda * lambda + epsilon * epsilon) / (2 * epsilon);
	}
}

// Max
double max(double a, double b)
{
	if (a > b)
	{
		return a;
	}
	return b;
}

// Flux
Eigen::Vector3d flux(Eigen::Vector3d Ql, Eigen::Vector3d Qr, Eigen::Vector3d n, int riemann_solver)
{

	double hl, ul, vl;
	double hr, ur, vr;
	double nx, ny, nz;
	Eigen::Vector3d delta, Fxl, Fxr, Fyl, Fyr, Fl, Fr, F, w;

	nx = n(0);
	ny = n(1);
	nz = n(2);

	// Difference between the right and left conservative variables
	delta = Qr - Ql;

	// Left primitive variables
	hl = Ql(0);
	ul = Ql(1) / Ql(0);
	vl = Ql(2) / Ql(0);

	// Right primitive variables
	hr = Qr(0);
	ur = Qr(1) / Qr(0);
	vr = Qr(2) / Qr(0);

	Fxl(0) = hl * ul;
	Fxl(1) = hl * ul * ul + g / 2 * hl * hl;
	Fxl(2) = hl * ul * vl;

	Fxr(0) = hr * ur;
	Fxr(1) = hr * ur * ur + g / 2 * hr * hr;
	Fxr(2) = hr * ur * vr;

	Fyl(0) = hl * vl;
	Fyl(1) = hl * vl * ul;
	Fyl(2) = hl * vl * vl + g / 2 * hl * hl;

	Fyr(0) = hr * vr;
	Fyr(1) = hr * vr * ur;
	Fyr(2) = hr * vr * vr + g / 2 * hr * hr;

	Fl = Fxl * nx + Fyl * ny;
	Fr = Fxr * nx + Fyr * ny;

	// Lax-Friedriechs flux
	//////////////////////////////////////////////////////////////////////

	if (riemann_solver == 0)
	{
		double cl, cr, wl, wr;

		cl = std::sqrt(g * hr);
		cr = std::sqrt(g * hr);
		wl = std::abs(ul * nx + vl * ny) + cl;
		wr = std::abs(ur * nx + vr * ny) + cr;
		w = max(wl, wr) * delta;
	}

	//Roe flux
	//////////////////////////////////////////////////////////////////////

	if (riemann_solver == 1)
	{

		double h, u, v, c, lambda1, lambda2, lambda3;
		Eigen::Vector3d r1, r2, r3, I1, I2, I3;

		h = 0.5 * (hl + hr);
		u = (std::sqrt(hl) * ul + std::sqrt(hr) * ur) / (std::sqrt(hl) + std::sqrt(hr));
		v = (std::sqrt(hl) * vl + std::sqrt(hr) * vr) / (std::sqrt(hl) + std::sqrt(hr));
		c = std::sqrt(g * h);

		// B = dFxdQ
		// C = dFydQ
		// A = B * nx + C * ny;
		// A = [ 0, nx, ny; g * h * nx - u * (u * nx + v * ny), u * nx + (u * nx + v * ny), u * ny; g * h * ny - v * (u * nx + v * ny), v * nx, v * ny + (u * nx + v * ny) ];

		// Eigenvalues of A
		lambda1 = u * nx + v * ny - c;
		lambda2 = u * nx + v * ny;
		lambda3 = u * nx + v * ny + c;

		// Left eigenvectors of A
		I1(0) = 0.5 + (u * nx + v * ny) / (2.0 * c);
		I1(1) = -nx / (2.0 * c);
		I1(2) = -ny / (2.0 * c);

		I2(0) = v * nx - u * ny;
		I2(1) = ny;
		I2(2) = -nx;

		I3(0) = 0.5 - (u * nx + v * ny) / (2.0 * c);
		I3(1) = nx / (2.0 * c);
		I3(2) = ny / (2.0 * c);

		// Right eigenvectors of A
		r1(0) = 1.0;
		r1(1) = u - c * nx;
		r1(2) = v - c * ny;

		r2(0) = 0.0;
		r2(1) = ny;
		r2(2) = -nx;

		r3(0) = 1.0;
		r3(1) = u + c * nx;
		r3(2) = v + c * ny;

		w = entropy_fix(lambda1) * I1.dot(delta) * r1 + entropy_fix(lambda2) * I2.dot(delta) * r2 + entropy_fix(lambda3) * I3.dot(delta) * r3;
	}

	F = 0.5 * (Fl + Fr) + 0.5 * w;

	return F;
}
