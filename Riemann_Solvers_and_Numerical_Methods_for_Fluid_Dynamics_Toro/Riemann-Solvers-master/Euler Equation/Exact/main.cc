#include <iostream>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <exception>
#include <cmath>
#include <algorithm>
#include <string>
#include <vector>

using namespace std;

const double TOL = 1e-6;

const double G0 = 1.4;
const double G1 = 0.5*(G0 - 1) / G0;
const double G2 = 0.5*(G0 + 1) / G0;
const double G3 = 2 * G0 / (G0 - 1);
const double G4 = 2 / (G0 - 1);
const double G5 = 2 / (G0 + 1);
const double G6 = (G0 - 1) / (G0 + 1);
const double G7 = (G0 - 1) / 2;
const double G8 = G0 - 1;
const double G9 = -G2;
const double G10 = -0.5*(G0 + 1) / pow(G0, 2);
const double G11 = -0.5*(3 * G0 + 1) / G0;
const double G12 = 1 / G0;

typedef struct
{
	double rho;
	double u;
	double p;

	//Auxiliary variables introduced for efficiency and convinence
	double a;
}PrimitiveVariable;

inline double sound_speed(double p, double rho)
{
	return sqrt(G0*p / rho);
}

inline double Ak(const PrimitiveVariable &Wk)
{
	return G5 / Wk.rho;
}

inline double Bk(const PrimitiveVariable &Wk)
{
	return G6 * Wk.p;
}

inline double gk(double p, const PrimitiveVariable &Wk)
{
	return sqrt(Ak(Wk) / (p + Bk(Wk)));
}

double fk(double p, const PrimitiveVariable &Wk)
{
	if (p > Wk.p)
	{
		//shock
		return (p - Wk.p)*gk(p, Wk);
	}
	else
	{
		//rarefraction
		return G4 * Wk.a*(pow(p / Wk.p, G1) - 1);
	}
}

double f(double p, const PrimitiveVariable &Wl, const PrimitiveVariable &Wr)
{
	return fk(p, Wl) + fk(p, Wr) + (Wr.u - Wl.u);
}

double dfk(double p, const PrimitiveVariable &Wk)
{
	if (p > Wk.p)
	{
		//shock
		double A = Ak(Wk);
		double B = Bk(Wk);
		return sqrt(A / (p + B))*(1 - 0.5*(p - Wk.p) / (B + p));
	}
	else
	{
		//rarefraction
		return pow(p / Wk.p, G9) / (Wk.rho*Wk.a);
	}
}

double df(double p, const PrimitiveVariable &Wl, const PrimitiveVariable &Wr)
{
	return dfk(p, Wl) + dfk(p, Wr);
}

double ddfk(double p, const PrimitiveVariable &Wk)
{
	if (p > Wk.p)
	{
		//shock
		double A = Ak(Wk);
		double B = Bk(Wk);
		return -0.25 * sqrt(A / (p + B)) * (4 * B + 3 * p + Wk.p) / pow(B + p, 2);
	}
	else
	{
		//rarefraction
		return G10 * Wk.a / pow(Wk.p, 2) * pow(p / Wk.p, G11);
	}
}

double ddf(double p, const PrimitiveVariable &Wl, const PrimitiveVariable &Wr)
{
	return ddfk(p, Wl) + ddfk(p, Wr);
}

double rhos(double ps, const PrimitiveVariable &Wk)
{
	double t = ps / Wk.p;

	if (ps > Wk.p)
		return Wk.rho * ((t + G6) / (G6*t + 1));
	else
		return Wk.rho * pow(t, G12);
}

void CalcWfanL(const PrimitiveVariable &Wk, double S, PrimitiveVariable &ans)
{
	ans.rho = Wk.rho * pow(G5 + G6 / Wk.a *(Wk.u - S), G4);
	ans.u = G5 * (Wk.a + G7 * Wk.u + S);
	ans.p = Wk.p * pow(G5 + G6 / Wk.a * (Wk.u - S), G3);
}

void CalcWfanR(const PrimitiveVariable &Wk, double S, PrimitiveVariable &ans)
{
	ans.rho = Wk.rho * pow(G5 - G6 / Wk.a *(Wk.u - S), G4);
	ans.u = G5 * (-Wk.a + G7 * Wk.u + S);
	ans.p = Wk.p * pow(G5 - G6 / Wk.a * (Wk.u - S), G3);
}

inline void SolSample(const PrimitiveVariable &Wl, const PrimitiveVariable &Wsl, const PrimitiveVariable &Wsr, const PrimitiveVariable &Wr, double S, PrimitiveVariable &ans)
{
	double u_star = Wsl.u;
	double S_L = Wl.u - Wl.a * sqrt(G2 * Wsl.p / Wl.p + G1);
	double S_HL = Wl.u - Wl.a;
	double S_TL = Wsl.u - Wsl.a;
	double S_R = Wr.u + Wr.a * sqrt(G2 * Wsr.p / Wr.p + G1);
	double S_HR = Wr.u + Wr.a;
	double S_TR = Wsr.u + Wsr.a;
	

	if (Wsl.p > Wl.p)
	{
		//Left Shock
		if (Wsr.p > Wr.p)
		{
			//Right Shock
			if (S < S_L)
				ans = Wl;
			else if (S < u_star)
				ans = Wsl;
			else if (S < S_R)
				ans = Wsr;
			else
				ans = Wr;
		}
		else
		{
			//Right Rarefaction
			if (S < S_L)
				ans = Wl;
			else if (S < u_star)
				ans = Wsl;
			else if (S < S_TR)
				ans = Wsr;
			else if (S < S_HR)
				CalcWfanR(Wr, S, ans);
			else
				ans = Wr;
		}
	}
	else
	{
		//Left Rarefaction
		if (Wsr.p > Wr.p)
		{
			//Right Shock
			if (S < S_HL)
				ans = Wl;
			else if (S < S_TL)
				CalcWfanL(Wl, S, ans);
			else if (S < u_star)
				ans = Wsl;
			else if (S < S_R)
				ans = Wsr;
			else
				ans = Wr;
		}
		else
		{
			//Right Rarefaction
			if (S < S_HL)
				ans = Wl;
			else if (S < S_TL)
				CalcWfanL(Wl, S, ans);
			else if (S < u_star)
				ans = Wsl;
			else if (S < S_TR)
				ans = Wsr;
			else if (S < S_HR)
				CalcWfanR(Wr, S, ans);
			else
				ans = Wr;
		}
	}
}

int main(int argc, char *argv[])
{
	int n = 0;
	cin >> n;
	for (int i = 1; i <= n; i++)
	{
		PrimitiveVariable Wl, Wr;
		double dt = 0.1;
		int NumOfStep = 201;

		//Input parameters
		try
		{
			cin >> Wl.rho >> Wl.u >> Wl.p;
			cin >> Wr.rho >> Wr.u >> Wr.p;
			cin >> dt >> NumOfStep;
		}
		catch (...)
		{
			cerr << "Invalid input!" << endl;
			return -1;
		}

		if (Wl.rho < 0 || Wr.rho < 0)
		{
			cerr << "Invalid data!" << endl;
			return -2;
		}

		//Check if vacuum or not
		int vacuum = -1;
		if (abs(Wl.rho) < 1e-7)
			vacuum = 1;
		else
			Wl.a = sound_speed(Wl.p, Wl.rho);
			
		if (abs(Wr.rho) < 1e-7)
			vacuum = 2;
		else
			Wr.a = sound_speed(Wr.p, Wr.rho);

		double du = Wr.u - Wl.u;
		if (vacuum < 0)
		{
			double du_crit = G4 * (Wl.a + Wr.a);
			if (!(du_crit > du))
				vacuum = 3;
			else
				vacuum = 0;
		}

		//Banner
		stringstream ss;
		ss << i;
		string header = "Case" + ss.str();

		cout.fill('=');
		cout << setw(32) << std::right << header << setw(32) << std::right << "" << endl;
		cout.fill(' ');

		//Time-steps
		vector<double> t_sample = vector<double>(NumOfStep);
		for (int n = 0; n < NumOfStep; n++)
			t_sample[n] = n * dt;

		//Coordinates
		double xl = -50, xr = 50;
		int NumOfLeftPnt = 250, NumOfRightPnt = 250;
		double dxl = -xl / NumOfLeftPnt, dxr = xr/NumOfRightPnt;
		int NumOfPnt = NumOfLeftPnt + NumOfRightPnt;
		vector<double> x_sample = vector<double>(NumOfPnt);
		for (int i = 0; i < NumOfLeftPnt; i++)
			x_sample[i] = xl + i * dxl;
		for (int i = 0; i < NumOfRightPnt; i++)
			x_sample[NumOfPnt - (i + 1)] = xr - i * dxr;
		
		//Output coordinates and intial settings
		ofstream fout(header + ".txt");
		if (!fout)
			throw "Failed to open file!";

		fout << NumOfStep << '\t' << NumOfPnt << endl;
		for (int i = 0; i < NumOfPnt; i++)
			fout << x_sample[i] << endl;
		fout << 0 << endl;
		for (int i = 0; i < NumOfPnt; i++)
		{
			double x = x_sample[i];
			if (x < 0)
				fout << Wl.rho << '\t' << Wl.u << '\t' << Wl.p << endl;
			else
				fout << Wr.rho << '\t' << Wr.u << '\t' << Wr.p << endl;
		}

		//Solve
		PrimitiveVariable local_ans;
		if (vacuum > 0)
		{
			cout << "Vacuum ";
			if(vacuum == 1)
			{
				cout << "on Left!" << endl;
				double SsR = Wr.u - G4 * Wr.a;
				double S_HR = Wr.u + Wr.a;

				cout << "\nGenerating animation data..." << endl;
				for (int n = 1; n < NumOfStep; n++)
				{
					fout << n << endl;
					double t = t_sample[n];
					for (int i = 0; i < NumOfPnt; i++)
					{
						double x = x_sample[i];
						double S = x / t;
						
						if (S <= SsR)
							local_ans = Wl;
						else if(S < S_HR)
							CalcWfanR(Wr, S, local_ans);
						else
							local_ans = Wr;

						fout << local_ans.rho << '\t' << local_ans.u << '\t' << local_ans.p << endl;
					}
				}
				cout << "Done!" << endl;
			}
			else if(vacuum == 2)
			{
				cout << "on Right!" << endl;

				double SsL = Wl.u + G4 * Wl.a;
				double S_HL = Wl.u - Wl.a;

				cout << "\nGenerating animation data..." << endl;
				for (int n = 1; n < NumOfStep; n++)
				{
					fout << n << endl;
					double t = t_sample[n];
					for (int i = 0; i < NumOfPnt; i++)
					{
						double x = x_sample[i];
						double S = x / t;
						
						if (S <= S_HL)
							local_ans = Wl;
						else if(S < SsL)
							CalcWfanL(Wl, S, local_ans);
						else
							local_ans = Wr;

						fout << local_ans.rho << '\t' << local_ans.u << '\t' << local_ans.p << endl;
					}
				}
				cout << "Done!" << endl;
			}
			else
			{
				cout << "in Between!" << endl;

				double SsL = Wl.u + G4 * Wl.a;
				double SsR = Wr.u - G4 * Wr.a;
				double S_HL = Wl.u - Wl.a;
				double S_HR = Wr.u + Wr.a;

				PrimitiveVariable W0;
				W0.rho = 0.0;
				W0.u = (Wl.u + Wr.u)/2;
				W0.p = 0.0;

				cout << "\nGenerating animation data..." << endl;
				for (int n = 1; n < NumOfStep; n++)
				{
					fout << n << endl;
					double t = t_sample[n];
					for (int i = 0; i < NumOfPnt; i++)
					{
						double x = x_sample[i];
						double S = x / t;
						
						if (S < S_HL)
							local_ans = Wl;
						else if(S < SsL)
							CalcWfanL(Wl, S, local_ans);
						else if(S < SsR)
							local_ans = W0;
						else if(S < S_HR)
							CalcWfanR(Wr, S, local_ans);
						else
							local_ans = Wr;

						fout << local_ans.rho << '\t' << local_ans.u << '\t' << local_ans.p << endl;
					}
				}
				cout << "Done!\n" << endl;
			}
		}
		else
		{		
			//Select initial pressure
			double p_min = min(Wl.p, Wr.p);
			double p_max = max(Wl.p, Wr.p);
			double f_min = f(p_min, Wl, Wr);
			double f_max = f(p_max, Wl, Wr);

			cout << setw(16) << std::left << "P_L" << setw(16) << std::left << "P_R" << setw(16) << std::left << "f_min" << setw(16) << std::left << "f_max" << endl;
			cout << setw(16) << std::left << Wl.p << setw(16) << std::left << Wr.p << setw(16) << std::left << f_min << setw(16) << std::left << f_max << endl;
			cout << endl;

			double p_m = (Wl.p + Wr.p) / 2;
			p_m = max(TOL, p_m);
			double p_pv = p_m - du * (Wl.rho + Wr.rho)*(Wl.a + Wr.a) / 8;
			p_pv = max(TOL, p_pv);
			double gL = gk(p_pv, Wl);
			double gR = gk(p_pv, Wr);
			double p_ts = (gL * Wl.p + gR * Wr.p - du) / (gL + gR);
			p_ts = max(TOL, p_ts);
			double p_tr = pow((Wl.a + Wr.a - G7 * du) / (Wl.a / pow(Wl.p, G1) + Wr.a / pow(Wr.p, G1)), G3);
			p_tr = max(TOL, p_tr);

			cout << setw(16) << std::left << "P_TR" << setw(16) << std::left << "P_PV" << setw(16) << std::left << "P_TS" << setw(16) << std::left << "P_M" << endl;
			cout << setw(16) << std::left << p_tr << setw(16) << std::left << p_pv << setw(16) << std::left << p_ts << setw(16) << std::left << p_m << endl;
			cout << endl;

			cout << "P0: ";
			double p0 = p_m;
			if (f_min < 0 && f_max < 0)
			{
				cout << "Two-Shock Approximation" << endl;
				p0 = p_ts;
			}
			else if (f_min > 0 && f_max > 0)
			{
				cout << "Two-Rarefraction Approximation" << endl;
				p0 = p_tr;
			}
			else
			{
				cout << "Intermediate Approximation" << endl;
				p0 = p_pv;
			}

			//Solve pressure
			cout << "\nSolving p* ..." << endl;
			int iter_cnt = 0;
			double CHA = 1.0;
			while (CHA > TOL)
			{
				++iter_cnt;

				double fder = df(p0, Wl, Wr);
				if (fder == 0)
					throw "Zero derivative!";

				double fval = f(p0, Wl, Wr);
				double fder2 = ddf(p0, Wl, Wr);
				double p = p0 - fval * fder / (pow(fder, 2) - 0.5*fval*fder2);
				if (p < 0)
				{
					p0 = TOL;
					break;
				}

				CHA = abs(2 * (p - p0) / (p + p0));
				p0 = p;
				cout << "Iter: " << iter_cnt << "  " << "P: " << p0 << endl;
			}
			cout << "Done!\n" << endl;

			PrimitiveVariable Wsl, Wsr;
			Wsl.p = Wsr.p = p0;
			Wsl.u = Wsr.u = (Wl.u + Wr.u) / 2 + (fk(p0, Wr) - fk(p0, Wl)) / 2;
			Wsl.rho = rhos(p0, Wl);
			Wsr.rho = rhos(p0, Wr);
			Wsl.a = sound_speed(Wsl.p, Wsl.rho);
			Wsr.a = sound_speed(Wsr.p, Wsr.rho);

			cout << setw(16) << std::left << "p*" << setw(16) << std::left << "u*" << setw(16) << std::left << "rho*L" << setw(16) << std::left << "rho*R" << endl;
			cout << setw(16) << std::left << p0 << setw(16) << std::left << Wsl.u << setw(16) << std::left << Wsl.rho << setw(16) << std::left << Wsr.rho << endl;

			cout << "\nGenerating animation data..." << endl;
			for (int n = 1; n < NumOfStep; n++)
			{
				fout << n << endl;
				double t = t_sample[n];
				for (int i = 0; i < NumOfPnt; i++)
				{
					double x = x_sample[i];
					double S = x / t;
					SolSample(Wl, Wsl, Wsr, Wr, S, local_ans);
					fout << local_ans.rho << '\t' << local_ans.u << '\t' << local_ans.p << endl;
				}
			}
			cout << "Done!" << endl;
		}
		fout.close();
	}

	cout.fill('=');
	cout << setw(32) << std::right << "End" << setw(32) << std::right << "" << endl;
	cout.fill(' ');

	return 0;
}
