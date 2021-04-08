#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <cmath>
#include <algorithm>
#include <cstddef>
#include <cassert>

using namespace std;

const size_t WIDTH = 16;
const size_t DIGITS = 6;

const double D = 1.0;
const int N = 21;
const double dy = D / (N - 1);
const double Re = 5000.0;
const double E = 10.0;
const double dt = E * Re * pow(dy, 2);
const double A = -E / 2;
const double B = 1 + E;

ofstream result;

vector<double> y(N, 0.0);
vector<double> u(N, 0.0);
vector<double> K(N, 0.0);

double t = 0.0;
int iter_cnt = 0;

void write_result(void)
{
	result << iter_cnt << '\t' << t << endl;
	for (int i = 0; i < N; ++i)
	{
		result << setw(WIDTH) << setprecision(DIGITS) << u[i];
		result << endl;
	}
}

void init(void)
{
	// Coordinate
	for (int j = 1; j < N; ++j)
		y[j] = y[j - 1] + dy;

	// Velocity field
	u[N - 1] = 1.0;

	// Header
	result.open("flow.txt");
	result << N << endl;
	for (int i = 0; i < N; ++i)
		result << y[i] << '\t';
	result << endl;
	write_result();
}

void finalize(void)
{
	result.close();
}

// Thomas algorithm for solving tri-diagnoal system
// a: Lower diagnoal, N-1 elements
// b: Middle diagnoal, no zero, N elements
// c: Upper diagnoal, N-1 elements
// d: RHS column vector, N elements
// f: Solution vector, N elements
void thomas(const vector<double>& a, const vector<double>& b, const vector<double>& c, const vector<double>& d, vector<double>& f)
{
	// Check length
	const size_t N = d.size();
	assert(N == a.size() + 1);
	assert(N == b.size());
	assert(N == c.size() + 1);
	assert(N == f.size());

	// Create the temporary vectors                                                                                                                                                                                                                                                                                                                                                          
	vector<double> c_star(N - 1, 0.0), d_star(N - 1, 0.0);

	// Updates the coefficients in the first row
	assert(b[0] != 0.0);
	c_star[0] = c[0] / b[0];
	d_star[0] = d[0] / b[0];

	// Create the c_star and d_star coefficients in the forward sweep                                                                                                                                                  
	for (size_t i = 1; i < N - 1; i++)
	{
		double m = 1.0 / (b[i] - a[i-1] * c_star[i - 1]);
		c_star[i] = c[i] * m;
		d_star[i] = (d[i] - a[i-1] * d_star[i - 1]) * m;
	}

	// The final line
	f[N - 1] = (d[N - 1] - a[N - 2] * d_star[N - 2]) / (b[N - 1] - a[N - 2] * c_star[N - 2]);

	// Reverse sweep, update the solution vector f                                                                                                                                                 
	for (int i = N - 2; i >= 0; i--)
		f[i] = d_star[i] - c_star[i] * f[i + 1];
}

void loop(void)
{
	// The RHS
	for (int j = 1; j < N - 1; ++j)
		K[j] = (1.0 - E) * u[j] + E * (u[j - 1] + u[j + 1]) / 2;

	K[1] -= A * u[0];
	K[N - 2] -= A * u[N - 1];

	// Solve the tri-diagnoal system
	static const vector<double> lower(N - 3, A), upper(N - 3, A), middle(N - 2, B);
	const vector<double> RHS(K.begin() + 1, K.end() - 1);
	vector<double> u_new(N - 2, 0.0);

	thomas(lower, middle, upper, RHS, u_new);

	// Update
	for (int j = 1; j < N - 1; ++j)
		u[j] = u_new[j - 1];

	t += dt;
}

bool check_convergence(void)
{
	double rms = 0.0;

	for (int j = 1; j < N - 1; ++j)
		rms += (u[j + 1] - 2 * u[j] + u[j - 1]) / pow(dy, 2);

	cout << "\tRMS = " << rms << endl;

	return abs(rms) < 1e-4 || iter_cnt > 2000;
}

void solve(void)
{
	bool converged = false;
	while (!converged)
	{
		++iter_cnt;
		cout << "Iter" << iter_cnt << ":" << endl;
		loop();
		write_result();
		converged = check_convergence();
	}
}

int main(int argc, char *argv[])
{
	init();
	solve();
	finalize();
	return 0;
}
