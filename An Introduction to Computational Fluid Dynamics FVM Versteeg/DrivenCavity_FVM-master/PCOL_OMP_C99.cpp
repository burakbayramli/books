// PCOL_OMP_C99.cpp : Defines the entry point for the console application.


/*{An in-house code which solves the steady Navier-Stokes equations,
   using the Finite Volume Method (FVM) in primitive variables formulation.
   Convective terms are discretized using second order central differencing scheme,
   and SIMPLE algorithm are used to decouple velocity and pressure. 
   Strongly Implicit Procedure was used to solve the resulted linear algebraic equations.}*/

/* Copyright 2016 Tamer A. Abdelmigid

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.*/


#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <Windows.h>
#include <omp.h>

using namespace std;

int _tmain(int argc, _TCHAR* argv[])
{

	////////////////
	//  Read Data //
	////////////////

	string Ofilename, DAT;
	ifstream INPUTFILE;
	INPUTFILE.open("Input.txt");

	omp_set_num_threads(12);

	double XMIN, XMAX;					// Positional limits of the solution domain
	double EXX;							// EXX: Grid Expansion Factor in x-direction
	int N=101;								// NI: No. of control volumes in x-direction
	int NI;							// NIM: No. of nodes is x-direction
	int NIM;
	double DX;							// DX: Distance between two consecutive nodes
	double YMIN, YMAX;					// Positional limits of the solution domain
	double EXY;							// EXY: Grid Expansion Factor in y-direction
	int M=101;								// NJ: No. of control volumes in y-direction
	int NJ;							// NJM: No. of nodes is y-direction
	int NJM;
	double DY;							// DY: Distance between two consecutive nodes

	double DEN = 1;							// DEN: Density
	double VIS = 0.001;							// VIS: Dynamic Viscosity
	double ULID = 1;						// ULID: Lid Velocity
	int MAXIT = 100;                          // MAXIT: Maximum No. of Outer Iteration
	int MAXITU = 1;                         // MAXITU: Maximum No. of Inner U velocity Iterations
	int MAXITV = 1;                         // MAXITU: Maximum No. of Inner V velocity Iterations
	int MAXITP = 6;                         // MAXITU: Maximum No. of Inner P Pressure Iterations
	int MAXITT = 1;                         // MAXITU: Maximum No. of Inner T Pressure Iterations
	double RESMAX = 0.00001;						// RESMAX: Maximum residual at which outer iterations to be stopped
	double RESMAXU = 0.2;						// RESMAXU: Maximum residual at which inner U iterations to be stopped
	double RESMAXV = 0.2;						// RESMAXV: Maximum residual at which inner V iterations to be stopped
	double RESMAXP = 0.01;						// RESMAXP: Maximum residual at which inner P iterations to be stopped
	double RESMAXT = 0.2;						// RESMAXP: Maximum residual at which inner T iterations to be stopped
	double SLARGE;						// SLARGE: Divergence Criteria
	double URFU = 0.8;						// URFU: Under-relaxation factor for U-velocity
	double URFV = 0.8;						// URFV: Under-relaxation factor for V-velocity
	double URFP = 0.3;						// URFP: Under-relaxation factor for P-Pressure
	double URFT = 0.9;						// URFP: Under-relaxation factor for T-Temperature
	double ALFA = 0.93;						// ALFA: Value needed by SIP Solver
	int IMON, JMON;						// IMON, JMON: X and Y coordinates of a point to monitor its values
	double PRM;							// PRM: Fluid Prandtl Number
	double GRAVX, GRAVY;				// GRAVX, GRAVY: Gravity in X and Y direction respectively
	double BETA;						// BETA: Fluid Volumetric expansion factor
	double TH, TC, TREF;				// TH, TC, TREF: Hot temperature, Cold temperature, and Reference Temperature
	int IPR, JPR;						// IPR, JPR: I and J indices of Pressure reference point
	bool LTIME;							// LTIME: True means Unsteady Simulation
	bool LCALIEN;						// LCALIEN: True Means Energy equation is solved
	double ITST;						// ITST: the number of time steps to be performed 
	// (1 if steady flow is considered)
	double NPRT;						// NPRT: Number of steps after which solution is saved
	double DT;							// DT: Time step size
	double TPER;						// TPER: The oscillation period in the case of unsteady flow
	// with oscillating lid.
	double GAMT;						// GAMT: the blending factor for time differencing schemes
	// (GAMT = 1->three time levels scheme, GAMT = 0->Euler implicit scheme).
	double GDSU, GDSV, GDSP, GDST;		// GDSU, GDSV, GDSP, GDST: the blending factor for UDS and CDS 
	// in the equation for U, V, P, T respectively 
	// (convective terms; value 1.0 means CDS(second order)
	// 0.0 means UDS(first order), any value between 0.0 and 1.0 can be used).
	// The value 1.0 is recommended, except for coarse grids,
	// in case convergence problems are encountered.
	double TIME;						// TIME: Time step
	double SOURCE, RESOR, RSMU, RSMV, RSMP, RSMT;
	double progress = 0.0;
	string SLTIME, SLCALIEN;
	string InterSave, InterRead;
	int InterSaveNo;

	XMIN = YMIN = 0;
	XMAX = YMAX = 1;


	/* Output File Name */
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE >> Ofilename;

	ofstream outputfile;
	ofstream resultfile;
	fstream Intermediate;
	outputfile.open(string(Ofilename + ".txt").c_str());


	/* Generate "ASCII" .dat File ? */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> DAT;

	if (DAT == "True" || DAT == "true")
	{
		resultfile.open(string(Ofilename + ".dat").c_str());
	}

	/* GRID */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> N >> M;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> EXX >> EXY;


	/* Discritization Settings */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> GDSU >> GDSV >> GDSP >> GDST;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> GAMT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> URFU >> URFV >> URFP >> URFT;


	/* Fluid Properties */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> DEN;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> VIS;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> BETA;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> PRM;


	/* Control */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> SLCALIEN;

	if (SLCALIEN == "True" || SLCALIEN == "true")
	{
		LCALIEN = true;
	}

	else
	{
		LCALIEN = false;
	}

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> SLTIME;

	if (SLTIME == "True" || SLTIME == "true")
	{
		LTIME = true;
	}

	else
	{
		LTIME = false;
	}


	/* Simulation Paramters */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> ULID;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> GRAVX;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> GRAVY;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> TH;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> TC;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> TREF;


	/* Time Controls */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> ITST;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> NPRT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> DT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> TPER;


	/* Monitoring Options */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> IMON >> JMON;


	/* Solver Criteria */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> MAXIT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> MAXITU >> MAXITV >> MAXITP >> MAXITT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> RESMAX;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> RESMAXU >> RESMAXV >> RESMAXP >> RESMAXT;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> SLARGE;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> ALFA;

	/* Intermediate Save */
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, ':');
	INPUTFILE.ignore(1024, '/');
	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> InterSave;

	if (InterSave == "True" || InterSave == "true")
	{

	}

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> InterSaveNo;

	INPUTFILE.ignore(1024, '|');
	INPUTFILE >> InterRead;

	/////////////////////
	// Define The Grid //
	/////////////////////

	

	/**************************************************************/

	NI = N + 2;
	NIM = NI - 1;

	double *X;
	X = (double*)malloc(NI*sizeof(double));			// X: Vector storing the X-position of nodes

	if (EXX == 1)
		DX = (XMAX - XMIN) / N;
	else
		DX = (XMAX - XMIN)*(1.0 - EXX) / (1 - pow(EXX, N));


	X[0] = XMIN;
	for (int i = 1; i < NIM; ++i)
	{
		X[i] = X[i - 1] + DX;
		DX = DX*EXX;
	}
	X[NI - 1] = X[NIM - 1];

	/* Coordinates of CV centers in x-direction */

	double *XC;
	XC = (double*)malloc(NI*sizeof(double));			// XC: Vector storing the X-position of cell centers
	

	XC[0] = X[0];
	for (int i = 1; i < NIM; ++i)
	{
		XC[i] = 0.5*(X[i] + X[i - 1]);
	}
	XC[NI - 1] = X[NIM - 1];

	/*for (int i = 0; i < NI; ++i)
	{
	outputfile << XC(i) << endl;
	}*/

	/**************************************************************/

	/* Define Y-Grid */


	NJ = M + 2;
	NJM = NJ - 1;

	double *Y;
	Y = (double*)malloc(NJ*sizeof(double));			// Y: Vector storing the Y-position of nodes

	

	if (EXY == 1)
		DY = (YMAX - YMIN) / M;
	else
		DY = (YMAX - YMIN)*(1.0 - EXY) / (1 - pow(EXY, M));


	Y[0] = YMIN;
	for (int i = 1; i < NJM; ++i)
	{
		Y[i] = Y[i - 1] + DY;
		DY = DY*EXY;
	}
	Y[NJ - 1] = Y[NJM - 1];



	/* Coordinates of CV centers in y-direction */

	double *YC;
	YC = (double*)malloc(NJ*sizeof(double));			// YC: Vector storing the Y-position of cell centers		

	YC[0] = Y[0];
	for (int j = 1; j < NJM; ++j)
	{
		YC[j] = 0.5*(Y[j] + Y[j - 1]);
	}
	YC[NJ - 1] = Y[NJM - 1];


	/**************************************************************/

	/* Set Radius R=1 for Plane */

	double *R;
	R = (double*)malloc(NJ*sizeof(double));				

	for (int j = 0; j < NJ; ++j)
	{
		R[j] = 1.0;
	}

	/**************************************************************/

	/* INTERPOLATION FACTORS (ON SCALAR CVs) */

	double *FX, *FY;
	FX = (double*)malloc(NI*sizeof(double));
	FY = (double*)malloc(NJ*sizeof(double));


	FX[0] = 0.0;
	FY[0] = 0.0;
	FX[NI - 1] = 0.0;
	FY[NJ - 1] = 0.0;

	for (int I = 1; I < NIM; ++I)
	{
		FX[I] = (X[I] - X[I - 1]) / (X[I + 1] - X[I - 1]);
	}

	for (int J = 1; J < NIM; ++J)
	{
		FY[J] = (Y[J] - Y[J - 1]) / (Y[J + 1] - Y[J - 1]);
	}


	//////////////////////////////////////////////////////////////////////////////////////////////////////////

	/////////////////////////
	// Data Initialization //
	/////////////////////////




	LARGE_INTEGER StartingTimeALL, EndingTimeALL, ElapsedMicrosecondsALL;
	LARGE_INTEGER StartingTimeU, EndingTimeU, ElapsedMicrosecondsU;
	LARGE_INTEGER StartingTimeV, EndingTimeV, ElapsedMicrosecondsV;
	LARGE_INTEGER StartingTimeP, EndingTimeP, ElapsedMicrosecondsP;
	LARGE_INTEGER StartingTimeT, EndingTimeT, ElapsedMicrosecondsT;
	LARGE_INTEGER Frequency;
	double TIMEU, TIMEV, TIMEP, TIMET;
	TIMEU = TIMEV = TIMEP = TIMET = 0.0;


	double SMALL = 0.000000000000000000000000000001;
	double LARGE = 100000000000000000;

	int IJMON;




	/* For Steady Lid Driven Cavity */

	//GRAVX = GRAVY = TH = TC = TREF = BETA = 0.0;
	//PRM = TPER = 1.0;
	//ITST = NPRT = 1.0;
	//DT = LARGE;
	//GAMT = 1.0;
	//GDSU = GDSV = GDSP = GDST = 1.0;
	//LTIME = false;
	//LCALIEN = false;
	IPR = JPR = 1;

	double PRR = 1.0 / PRM;
	double OM = 8.0 * atan(1.0) / TPER;
	double DTR = 1.0 / DT;
	double URFUR, URFVR, URFPR, URFTR;
	URFPR = 1.0 / URFP;

	TIME = 0.0;

	///////////////////////////////////////////////////////////////////////////////////////////////

	int *LI;
	LI = (int*)malloc(NJ*sizeof(int));

	for (int I = 0; I < NI; ++I)
	{
		LI[I] = I*NJ;
	}

	IJMON = LI[IMON] + JMON;


	double NIJ = NI*NJ;

	double *U, *V, *P, *T;
	U = (double*)malloc(NIJ*sizeof(double));
	V = (double*)malloc(NIJ*sizeof(double));
	P = (double*)malloc(NIJ*sizeof(double));
	T = (double*)malloc(NIJ*sizeof(double));


	for (int i = 0; i < NIJ; ++i)
	{
		U[i] = V[i] = P[i] = T[i] = 0.0;
	}

	double *UO, *VO, *PO, *TO;
	UO = (double*)malloc(NIJ*sizeof(double));
	VO = (double*)malloc(NIJ*sizeof(double));
	PO = (double*)malloc(NIJ*sizeof(double));
	TO = (double*)malloc(NIJ*sizeof(double));

	double *UOO, *VOO, *POO, *TOO;
	UOO = (double*)malloc(NIJ*sizeof(double));
	VOO = (double*)malloc(NIJ*sizeof(double));
	POO = (double*)malloc(NIJ*sizeof(double));
	TOO = (double*)malloc(NIJ*sizeof(double));
	
	double *AN, *AS, *AE, *AW, *AP;
	AN = (double*)malloc(NIJ*sizeof(double));
	AS = (double*)malloc(NIJ*sizeof(double));
	AE = (double*)malloc(NIJ*sizeof(double));
	AW = (double*)malloc(NIJ*sizeof(double));
	AP = (double*)malloc(NIJ*sizeof(double));

	double *UE, *UN, *LW, *LS, *LES, *RES, *LPR;
	UE = (double*)malloc(NIJ*sizeof(double));
	UN = (double*)malloc(NIJ*sizeof(double));
	LW = (double*)malloc(NIJ*sizeof(double));
	LS = (double*)malloc(NIJ*sizeof(double));
	LES = (double*)malloc(NIJ*sizeof(double));
	RES = (double*)malloc(NIJ*sizeof(double));
	LPR = (double*)malloc(NIJ*sizeof(double));

	double P1, P2, RESL, RSM;

	/*******************************************/

	double FXE, FXP, DXPE, S, D, CE, CP, CN;
	double FYN, FYP, DYPN;
	double FUUDS, FVUDS, FUCDS, FVCDS;
	double RP, VOL;
	double PE, PW, PS, PN, SB;
	double APT;

	double *F1, *F2, *DPX, *DPY;
	F1 = (double*)malloc(NIJ*sizeof(double));
	F2 = (double*)malloc(NIJ*sizeof(double));
	DPX = (double*)malloc(NIJ*sizeof(double));
	DPY = (double*)malloc(NIJ*sizeof(double));

	/*******************************************/

	double *SU, *SV, *APU, *APV;
	SU = (double*)malloc(NIJ*sizeof(double));
	SV = (double*)malloc(NIJ*sizeof(double));
	APU = (double*)malloc(NIJ*sizeof(double));
	APV = (double*)malloc(NIJ*sizeof(double));

	/*******************************************/

	double VOLE, DPXEL, UEL, APUE, DPXE, UEP;
	double VOLN, DPYNL, VNL, APVN, DPYN, VN;
	double PPO;
	int IJPREF;
	double PPE, PPW, PPN, PPS;

	double SUM;

	double *PP;
	PP = (double*)malloc(NIJ*sizeof(double));

	/*******************************************/

	double FUDS, FCDS;


	/////////////////////////////////////
	// BOUNDARY AND INITIAL CONDITIONS //
	/////////////////////////////////////

	/* WEST AND EAST ISOTHERMAL BOUNDARIES */

	for (int J = 0; J < NJ; ++J)
	{
		T[J] = TH;
	}

	for (int J = 0; J < NJ; ++J)
	{
		T[(LI[NI - 1] + J)] = TC;
	}

	/* NORTH WALL VELOCITY (FOR LID-DRIVEN CAVITY) */

	if (LTIME)
	{
		for (int I = 1; I < NIM; ++I)
		{
			int IJ = LI[I] + NJ - 1;
			U[IJ] = ULID*sin(OM*TIME);
		}
	}

	else
	{
		for (int I = 1; I < NIM; ++I)
		{
			int IJ = LI[I] + NJ - 1;
			U[IJ] = ULID;
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////

	outputfile << "                               PREDICTION OF FLOW IN LID DRIVEN CAVITIES " << endl;
	outputfile << "                               ========================================= " << endl;
	outputfile << "                               Reynolds Number     : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << (ULID*DEN*1.0) / VIS << endl;
	outputfile << "                               Prandtl Number      : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << PRM << endl;
	outputfile << "                               Fluid Density       : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << DEN << endl;
	outputfile << "                               Dynamic Viscosity   : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << VIS << endl;
	outputfile << endl << endl;

	outputfile << "                               Gravity in X-Dir.     : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GRAVX << endl;
	outputfile << "                               Gravity in Y-Dir.     : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GRAVY << endl;
	outputfile << "                               Hot Wall Temperature  : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << TH << endl;
	outputfile << "                               Cold Wall Temperature : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << TC << endl;
	outputfile << endl << endl;

	outputfile << "                               U - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFU << endl;
	outputfile << "                               V - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFV << endl;
	outputfile << "                               P - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFP << endl;
	outputfile << "                               T - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFT << endl;
	outputfile << endl << endl;

	outputfile << "                               U - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSU << endl;
	outputfile << "                               V - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSV << endl;
	outputfile << "                               P - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSP << endl;
	outputfile << "                               T - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDST << endl;
	outputfile << endl << endl;

	outputfile << "                               NO of Nodes in X-Direction : "
		<< setw(3) << fixed << left << setfill(' ') << N << endl;
	outputfile << "                               NO of Nodes in Y-Direction : "
		<< setw(3) << fixed << left << setfill(' ') << M << endl;
	outputfile << endl << endl;

	outputfile << "                               Alfa Parameter : "
		<< setw(3) << fixed << left << setfill(' ') << ALFA << endl;
	outputfile << endl << endl;

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	cout << "                    PREDICTION OF FLOW IN LID DRIVEN CAVITIES " << endl;
	cout << "                    ========================================= " << endl;
	cout << "                    Reynolds Number     : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << (ULID*DEN*1.0) / VIS << endl;
	cout << "                    Prandtl Number      : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << PRM << endl;
	cout << "                    Fluid Density       : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << DEN << endl;
	cout << "                    Dynamic Viscosity   : "
		<< setw(5) << scientific << setprecision(3) << left << setfill('0') << VIS << endl;
	cout << endl;

	cout << "                    U - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFU << endl;
	cout << "                    V - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFV << endl;
	cout << "                    P - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFP << endl;
	cout << "                    T - Under Relaxation Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << URFT << endl;
	cout << endl;

	cout << "                    U - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSU << endl;
	cout << "                    V - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSV << endl;
	cout << "                    P - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDSP << endl;
	cout << "                    T - Blending Factor : "
		<< setw(5) << fixed << setprecision(2) << left << setfill(' ') << GDST << endl;
	cout << endl;

	cout << "                    NO of Nodes in X-Direction : "
		<< setw(3) << fixed << left << setfill(' ') << N << endl;
	cout << "                    NO of Nodes in Y-Direction : "
		<< setw(3) << fixed << left << setfill(' ') << M << endl;
	cout << endl;

	cout << "                    Alfa Parameter : "
		<< setw(3) << fixed << left << setfill(' ') << ALFA << endl;
	cout << endl;


	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


	/****************************************************/

	QueryPerformanceFrequency(&Frequency);
	QueryPerformanceCounter(&StartingTimeALL);

	/***************************************************/


	///////////////
	// Time Loop //
	///////////////

	double ITIMS = 0.0;
	double ITIME = ITIMS + ITST;

	for (double ITIM = ITIMS; ITIM < ITIME; ++ITIM)
	{
		TIME = TIME + DT;

		/* SHIFT SOLUTIONS IN TIME */

		if (LTIME)
		{
			for (int IJ = 0; IJ < NIJ; ++IJ)
			{
				TOO[IJ] = TO[IJ];
				UOO[IJ] = UO[IJ];
				VOO[IJ] = VO[IJ];
				TO[IJ] = T[IJ];
				UO[IJ] = U[IJ];
				VO[IJ] = V[IJ];
			}
		}

		outputfile << "TIME = " << TIME << endl;
		outputfile << "===================" << endl;
		outputfile << "ITER    I-------------ABSOLUTE RESIDUAL SOURCE SUMS-------------I      I---------FIELD VALUES AT LOCATION ("
			<< fixed << left << IMON << "," << fixed << left << JMON << ")---------I" << endl;
		outputfile << "--------------------------------------------------------------------------------------------------------------------------------------------" << endl;
		outputfile << "NO     |     RSMU     |     RSMV     |     RSMP     |     RSMT     ||       UMON       |       VMON       |      PMON       |     TMON" << endl;
		outputfile << "--------------------------------------------------------------------------------------------------------------------------------------------" << endl;


		if (LTIME)
		{
			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I] + NJ - 1;
				U[IJ] = ULID*sin(OM*TIME);
			}
		}

		///////////////////////////////////////////
		// OUTER ITERATIONS (SIMPLE RELAXATIONS) //
		///////////////////////////////////////////

		for (int ITER = 0; ITER < MAXIT; ++ITER)
		{

			if (InterRead == "True" || InterRead == "true")
			{
				if (ITER == 0)
				{
					Intermediate.open(string(Ofilename + ".inter").c_str(), ios::in);
					Intermediate >> ITER;
					ITER = 0;
					while (!Intermediate.eof())
					{
						int I, J;
						Intermediate >> I >> J;
						int IJ = LI[I] + J;
						Intermediate >> U[IJ] >> V[IJ] >> P[IJ] >> T[IJ];
					}
					Intermediate.close();
				}
			}

			////////////
			// CALCUV //
			////////////




			/* This routine sets the coefficient matrix for the U and
			V equations, and calls the linear equation solver to
			update the velocity components. Constant fluid
			properties are assumed */

			/* RECIPROCAL VALUES OF UNDER-RELAXATION FACTORS FOR U AND V */

			URFUR = 1.0 / URFU;
			URFVR = 1.0 / URFV;

			/* SET BOUNDARY PRESSURE (LINEAR EXTRAPOLATION FROM INSIDE) */

			/*--------------------------------------------------------------------------------*/

			/* PBOUND */

			/* This routine calculates boundary values of pressure or
			pressure-correction by extrapolating (linearly) from inside.*/

			/* SOUTH AND NORTH BOUNDARIES */

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I];
				P[IJ] = P[IJ + 1] + (P[IJ + 1] - P[IJ + 2])*FY[1];
				IJ = LI[I] + NJ - 1;
				P[IJ] = P[IJ - 1] + (P[IJ - 1] - P[IJ - 2])*(1.0 - FY[NJM - 1 - 1]);
			}

			/* WEST AND EAST BOUNDARIES */

			int NJ2 = 2 * NJ;

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[0] + J;
				P[IJ] = P[IJ + NJ] + (P[IJ + NJ] - P[IJ + NJ2])*FX[1];
				IJ = LI[NI - 1] + J;
				P[IJ] = P[IJ - NJ] + (P[IJ - NJ] - P[IJ - NJ2])*(1.0 - FX[NIM - 1 - 1]);
			}

			/*--------------------------------------------------------------------------------*/

			/* INITIALIZE TEMPORARILY STORED VARIABLES */

			for (int IJ = 0; IJ < NIJ; ++IJ)
			{
				SU[IJ] = 0.0;
				SV[IJ] = 0.0;
				APU[IJ] = 0.0;
				APV[IJ] = 0.0;
			}

			/* FLUXES THROUGH INTERNAL EAST CV FACES */

			/* F1(IJ) is the mass flux through the east face (outward normal directed to E);
			FX(I) is the ratio of distance from P to cell face, to distance from P to E; IJ
			denotes node P and IJE node E. Contribution of convective and diffusive fluxes
			from east face to AE(P), AW(E), and source terms at both P and E are calculated;
			contributions to AP(P) and AP(E) come through the sum of neighbor coefficients
			and are not explicitly calculated.*/

			for (int I = 1; I < NIM - 1; ++I)
			{
				/* INTERPOLATION FACTORS, DISTANCE FROM P TO E (SAME FOR ALL J) */

				FXE = FX[I];
				FXP = 1.0 - FXE;
				DXPE = XC[I + 1] - XC[I];

				for (int J = 1; J < NJM; ++J)
				{
					int IJ = LI[I] + J;
					int IJE = IJ + NJ;

					/* CELL FACE AREA S = DY*RE*1 */

					S = (Y[J] - Y[J - 1])*(R[J] + R[J - 1])*0.5;

					/* COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V) */

					D = VIS*S / DXPE;

					/* EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS */

					CE = min(F1[IJ], 0.0);
					CP = max(F1[IJ], 0.0);

					FUUDS = CP*U[IJ] + CE*U[IJE];
					FVUDS = CP*V[IJ] + CE*V[IJE];
					FUCDS = F1[IJ]*(U[IJE]*FXE + U[IJ]*FXP);
					FVCDS = F1[IJ]*(V[IJE]*FXE + V[IJ]*FXP);

					/* COEFFICIENTS AE(P) AND AW(E) DUE TO UDS */

					AE[IJ] = CE - D;
					AW[IJE] = -CP - D;

					/* SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION */

					SU[IJ] = SU[IJ] + GDSU*(FUUDS - FUCDS);
					SU[IJE] = SU[IJE] - GDSU*(FUUDS - FUCDS);
					SV[IJ] = SV[IJ] + GDSU*(FVUDS - FVCDS);
					SV[IJE] = SV[IJE] - GDSU*(FVUDS - FVCDS);
				}
			}

			/* FLUXES THROUGH INTERNAL NORTH CV FACES */

			/* F2(IJ) is the mass flux through the north face (outward normal directed to N);
			FY(J) is the ratio of distance from P to cell face, to distance from P to N;
			IJ denotes node P and IJN node N.Contribution of convective and diffusive fluxes
			from north face to AN(P), AS(N), and source terms at both P and N are calculated;
			contributions to AP(P) and AP(N) come through the sum of neighbor coefficients
			and are not explicitly calculated.*/

			for (int J = 1; J < NJM - 1; ++J)
			{
				/* INTERPOLATION FACTORS, DISTANCE FROM P TO N (SAME FOR ALL J) */

				FYN = FY[J];
				FYP = 1.0 - FYN;
				DYPN = YC[J + 1] - YC[J];

				for (int I = 1; I < NIM; ++I)
				{
					int IJ = LI[I] + J;
					int IJN = IJ + 1;

					/* CELL FACE AREA S = DX*RN*1 */

					S = (X[I] - X[I - 1])*R[J];

					/* COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V) */

					D = VIS*S / DYPN;

					/* EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS */

					CN = min(F2[IJ], 0.0);
					CP = max(F2[IJ], 0.0);

					FVUDS = CP*V[IJ] + CN*V[IJN];
					FUUDS = CP*U[IJ] + CN*U[IJN];
					FUCDS = F2[IJ]*(U[IJN]*FYN + U[IJ]*FYP);
					FVCDS = F2[IJ]*(V[IJN]*FYN + V[IJ]*FYP);

					/* COEFFICIENTS AN(P) AND AS(N) DUE TO UDS */

					AN[IJ] = CN - D;
					AS[IJN] = -CP - D;

					/* SOURCE TERM CONTRIBUTIONS AT P AND N DUE TO DEFERRED CORRECTION */

					SU[IJ] = SU[IJ] + GDSU*(FUUDS - FUCDS);
					SU[IJN] = SU[IJN] - GDSU*(FUUDS - FUCDS);
					SV[IJ] = SV[IJ] + GDSU*(FVUDS - FVCDS);
					SV[IJN] = SV[IJN] - GDSU*(FVUDS - FVCDS);
				}
			}

			/* VOLUME INTEGRALS (SOURCE TERMS) */

			/* Cell-face pressure calculated using linear interpolation;
			cell volume is VOL, RP is the radius at node P; DX and DY
			are the width and height of the cell. Contribution to AP
			coefficient from volume integrals is stored temporarily
			in arrays APU and APV for U and V, respectively; these
			arrays are later used to store 1/AP, which is needed in
			the pressure-correction equation. */

			for (int I = 1; I < NIM; ++I)
			{
				DX = X[I] - X[I - 1];

				for (int J = 1; J < NJM; ++J)
				{
					DY = Y[J] - Y[J - 1];
					RP = 0.5*(R[J] + R[J - 1]);
					VOL = DX*DY*RP;
					int IJ = LI[I] + J;

					/* CELL-FACE PRESSURE, CELL-CENTER GRADIENT, SOURCE */

					PE = P[IJ + NJ]*FX[I] + P[IJ]*(1.0 - FX[I]);
					PW = P[IJ]*FX[I - 1] + P[IJ - NJ]*(1.0 - FX[I - 1]);
					PN = P[IJ + 1]*FY[J] + P[IJ]*(1.0 - FY[J]);
					PS = P[IJ]*FY[J - 1] + P[IJ - 1]*(1.0 - FY[J - 1]);
					DPX[IJ] = (PE - PW) / DX;
					DPY[IJ] = (PN - PS) / DY;
					SU[IJ] = SU[IJ] - DPX[IJ]*VOL;
					SV[IJ] = SV[IJ] - DPY[IJ]*VOL;

					/* BUOYANCY SOURCE CONTRIBUTION */

					if (LCALIEN)
					{
						SB = -BETA*DEN*VOL*(T[IJ] - TREF);
						SU[IJ] = SU[IJ] + GRAVX*SB;
						SV[IJ] = SV[IJ] + GRAVY*SB;
					}

					/* AXISYMMETRIC CONTRIBUTION */

					//IF(LAXIS) THEN
					//APV(IJ) = APV(IJ) + VISC*VOL / RP**2
					//ENDIF

					/* UNSTEADY TERM CONTRIBUTION TO AP AND SU */

					if (LTIME)
					{
						APT = DEN*VOL*DTR;
						SU[IJ] = SU[IJ] + (1.0 + GAMT)*APT*UO[IJ] - 0.5*GAMT*APT*UOO[IJ];
						SV[IJ] = SV[IJ] + (1.0 + GAMT)*APT*VO[IJ] - 0.5*GAMT*APT*VOO[IJ];
						APV[IJ] = APV[IJ] + (1.0 + 0.5*GAMT)*APT;
						APU[IJ] = APU[IJ] + (1.0 + 0.5*GAMT)*APT;
					}
				}
			}

			/* PROBLEM MODIFICATIONS - BOUNDARY CONDITIONS */

			/*******************************************************************************/

			/* BCUV */

			/*  In this routine, boundary conditions for U and V equations
			are implemented, i.e. fluxes through boundary cell faces
			are approximated. Here, the boundaries encountered in
			cavity flows are considered; inlet and outlet boundaries
			require different treatment*/

			/* SOUTH BOUNDARY (WALL; SHEAR FORCE IN X-DIR, DV/DY=0) */

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I] + 1;
				D = VIS*(X[I] - X[I - 1])*R[0] / (YC[1] - YC[0]);
				APU[IJ] = APU[IJ] + D;
				SU[IJ] = SU[IJ] + D*U[IJ - 1];
			}

			/* NORTH BOUNDARY (WALL, SHEAR FORCE IN X-DIR, DV/DY=0)*/

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I] + NJM - 1;
				D = VIS*(X[I] - X[I - 1])*R[NJM - 1] / (YC[NJ - 1] - YC[NJM - 1]);
				APU[IJ] = APU[IJ] + D;
				SU[IJ] = SU[IJ] + D*U[IJ + 1];
			}

			/* WEST BOUNDARY (WALL, SHEAR FORCE IN Y-DIR, DU/DX=0) */

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[1] + J;
				D = 0.5*VIS*(Y[J] - Y[J - 1])*(R[J] + R[J - 1]) / (XC[1] - XC[0]);
				APV[IJ] = APV[IJ] + D;
				SV[IJ] = SV[IJ] + D*V[IJ - NJ];
			}

			/* EAST BOUNDARY (WALL, SHEAR FORCE IN Y-DIR, DU/DX=0) */

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[NIM - 1] + J;
				D = 0.5*VIS*(Y[J] - Y[J - 1])*(R[J] + R[J - 1]) / (XC[NI - 1] - XC[NIM - 1]);
				APV[IJ] = APV[IJ] + D;
				SV[IJ] = SV[IJ] + D*V[IJ + NJ];
			}

			/*******************************************************************************/

			/* UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR U-VELOCITY */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					AP[IJ] = (-AE[IJ] - AW[IJ] - AN[IJ] - AS[IJ] + APU[IJ])*URFUR;
					SU[IJ] = SU[IJ] + (1.0 - URFU)*AP[IJ]*U[IJ];
					APU[IJ] = 1.0 / AP[IJ];
				}
			}

			/*******************************************************************************/

			/****************************************************/

			QueryPerformanceFrequency(&Frequency);
			QueryPerformanceCounter(&StartingTimeU);

			/***************************************************/

			////////////
			// SIPSOL //
			////////////

			/* COEFFICIENTS OF UPPER AND LOWER TRIANGULAR MATRICES */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					LW[IJ] = AW[IJ] / (1.0 + ALFA*UN[IJ - NJ]);
					LS[IJ] = AS[IJ] / (1.0 + ALFA*UE[IJ - 1]);
					P1 = ALFA*LW[IJ]*UN[IJ - NJ];
					P2 = ALFA*LS[IJ]*UE[IJ - 1];
					LPR[IJ] = 1.0 / (AP[IJ] + P1 + P2 - LW[IJ]*UE[IJ - NJ] - LS[IJ]*UN[IJ - 1]);
					UN[IJ] = (AN[IJ] - P1)*LPR[IJ];
					UE[IJ] = (AE[IJ] - P2)*LPR[IJ];
				}
			}

			/* INNER ITERATIONS LOOP*/

			for (int L = 0; L < MAXITU; ++L)
			{
				RESL = 0.0;

				/* CALCULATE RESIDUAL AND OVERWRITE IT BY INTERMEDIATE VECTOR */

				for (int I = 1; I < NIM; ++I)
				{
					int Start = (LI[I] + 1);
					int End = (LI[I] + NJM);
#pragma omp parallel for reduction (+:RESL) 
					for (int IJ = Start; IJ < End; ++IJ)
					{
						RES[IJ] = SU[IJ] - AN[IJ]*U[IJ + 1] - AS[IJ]*U[IJ - 1] -
							AE[IJ]*U[IJ + NJ] - AW[IJ]*U[IJ - NJ] - AP[IJ]*U[IJ];

						double ABS = abs(RES[IJ]);
						RESL += ABS;
					}
				}

				for (int J = 2; J <= (NI + NI - 2); ++J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50) 
					for (int I = Start; I <= End; ++I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = (RES[IJ] - LS[IJ]*RES[IJ - 1] - LW[IJ]*RES[IJ - NJ])*LPR[IJ];
					}
				}


				/* STORE INITIAL RESIDUAL SUM FOR CHECKING CONV. OF OUTER ITER. */

				if (L == 0)
				{
					RESOR = RESL;
				}

				RSM = RESL / (RESOR + SMALL);

				/* BACK SUBSTITUTION AND CORRECTION */

				for (int J = (NI + NI - 2); J >= 2; --J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50)
					for (int I = End; I >= Start; --I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = RES[IJ] - UN[IJ]*RES[IJ + 1] - UE[IJ]*RES[IJ + NJ];
						U[IJ] = U[IJ] + RES[IJ];
					}
				}				

				RSMU = RESOR;

				if (RSM < RESMAXU)
				{
					L = MAXITU;
				}

			}

			/*******************************************************************************/



			QueryPerformanceCounter(&EndingTimeU);
			ElapsedMicrosecondsU.QuadPart = EndingTimeU.QuadPart - StartingTimeU.QuadPart;
			ElapsedMicrosecondsU.QuadPart *= 1000000;
			ElapsedMicrosecondsU.QuadPart /= Frequency.QuadPart;
			TIMEU += ElapsedMicrosecondsU.QuadPart;

			/*********************************************************************************/

			/* UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR V-VELOCITY */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					AP[IJ] = (-AE[IJ] - AW[IJ] - AN[IJ] - AS[IJ] + APV[IJ])*URFVR;
					SU[IJ] = SV[IJ] + (1.0 - URFV)*AP[IJ]*V[IJ];
					APV[IJ] = 1.0 / AP[IJ];
				}
			}

			/*******************************************************************************/

			/****************************************************/

			QueryPerformanceFrequency(&Frequency);
			QueryPerformanceCounter(&StartingTimeV);

			/***************************************************/


			////////////
			// SIPSOL //
			////////////

			/* COEFFICIENTS OF UPPER AND LOWER TRIANGULAR MATRICES */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					LW[IJ] = AW[IJ] / (1.0 + ALFA*UN[IJ - NJ]);
					LS[IJ] = AS[IJ] / (1.0 + ALFA*UE[IJ - 1]);
					P1 = ALFA*LW[IJ]*UN[IJ - NJ];
					P2 = ALFA*LS[IJ]*UE[IJ - 1];
					LPR[IJ] = 1.0 / (AP[IJ] + P1 + P2 - LW[IJ]*UE[IJ - NJ] - LS[IJ]*UN[IJ - 1]);
					UN[IJ] = (AN[IJ] - P1)*LPR[IJ];
					UE[IJ] = (AE[IJ] - P2)*LPR[IJ];
				}
			}

			/* INNER ITERATIONS LOOP*/

			for (int L = 0; L < MAXITV; ++L)
			{
				RESL = 0.0;

				/* CALCULATE RESIDUAL AND OVERWRITE IT BY INTERMEDIATE VECTOR */

				for (int I = 1; I < NIM; ++I)
				{
					int Start = (LI[I] + 1);
					int End = (LI[I] + NJM);
#pragma omp parallel for reduction (+:RESL)
					for (int IJ = Start; IJ < End; ++IJ)
					{
						RES[IJ] = SU[IJ] - AN[IJ]*V[IJ + 1] - AS[IJ]*V[IJ - 1] -
							AE[IJ]*V[IJ + NJ] - AW[IJ]*V[IJ - NJ] - AP[IJ]*V[IJ];

						double ABS = abs(RES[IJ]);
						RESL += ABS;
					}
				}

				for (int J = 2; J <= (NI + NI - 2); ++J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50) 
					for (int I = Start; I <= End; ++I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = (RES[IJ] - LS[IJ]*RES[IJ - 1] - LW[IJ]*RES[IJ - NJ])*LPR[IJ];
					}
				}


				/* STORE INITIAL RESIDUAL SUM FOR CHECKING CONV. OF OUTER ITER. */

				if (L == 0)
				{
					RESOR = RESL;
				}

				RSM = RESL / (RESOR + SMALL);

				/* BACK SUBSTITUTION AND CORRECTION */

				for (int J = (NI + NI - 2); J >= 2; --J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50)
					for (int I = End; I >= Start; --I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = RES[IJ] - UN[IJ]*RES[IJ + 1] - UE[IJ]*RES[IJ + NJ];
						V[IJ] = V[IJ] + RES[IJ];
					}
				}				

				RSMV = RESOR;

				if (RSM < RESMAXV)
				{
					L = MAXITV;
				}

			}

			/*******************************************************************************/



			QueryPerformanceCounter(&EndingTimeV);
			ElapsedMicrosecondsV.QuadPart = EndingTimeV.QuadPart - StartingTimeV.QuadPart;
			ElapsedMicrosecondsV.QuadPart *= 1000000;
			ElapsedMicrosecondsV.QuadPart /= Frequency.QuadPart;
			TIMEV += ElapsedMicrosecondsV.QuadPart;

			/*********************************************************************************/

			///////////
			// CALCP //
			///////////



			/* This routine assembles and solves the pressure-correction
			equation. Cell-face values of velocity components, used to
			calculate the mass fluxes, are obtained by linear interpolation
			and then corrected by adding a term proportional to the third derivative
			of pressure and squared grid spacing, */

			/* EAST CV FACES (S - AREA, VOLE - VOLUME BETWEEN P AND E) */

			for (int I = 1; I < NIM - 1; ++I)
			{
				DXPE = XC[I + 1] - XC[I];
				FXE = FX[I];
				FXP = 1.0 - FXE;

				for (int J = 1; J < NJM; ++J)
				{
					int IJ = LI[I] + J;
					int	IJE = IJ + NJ;

					S = (Y[J] - Y[J - 1])*(R[J] + R[J - 1])*0.5;
					VOLE = DXPE*S;
					D = DEN*S;

					/* INTERPOLATED CELL FACE QUANTITIES (PRESSURE GRAD., U AND 1/AP)
					Note: pressure gradient is interpolated midway between P and E,
					since the gradient calculated at cell face is second order
					accurate at that location; the velocity is interpolated linearly,
					to achieve second order accuracy at cell face center. */

					DPXEL = 0.5*(DPX[IJE] + DPX[IJ]);
					UEL = U[IJE]*FXE + U[IJ]*FXP;
					APUE = APU[IJE]*FXE + APU[IJ]*FXP;

					/* CELL FACE GRADIENT, VELOCITY AND MASS FLUX */

					DPXE = (P[IJE] - P[IJ]) / DXPE;
					UEP = UEL - APUE*VOLE*(DPXE - DPXEL);
					F1[IJ] = D*UEP;

					/* COEFFICIENTS OF P' EQUATION, AE(P) AND AW(E) */

					AE[IJ] = -D*APUE*S;
					AW[IJE] = AE[IJ];
				}
			}

			/* NORTH CV FACES (S - AREA, VOLN - VOLUME BETWEEN P AND N) */

			for (int J = 1; J < NJM - 1; ++J)
			{
				DYPN = YC[J + 1] - YC[J];
				FYN = FY[J];
				FYP = 1.0 - FYN;

				for (int I = 1; I < NIM; ++I)
				{
					int IJ = LI[I] + J;
					int IJN = IJ + 1;

					S = (X[I] - X[I - 1])*R[J];
					VOLN = S*DYPN;
					D = DEN*S;

					/* INTERPOLATED CELL-FACE QUANTITIES (PRESSURE GRAD., U AND 1/AP) */

					DPYNL = 0.5*(DPY[IJN] + DPY[IJ]);
					VNL = V[IJN]*FYN + V[IJ]*FYP;
					APVN = APV[IJN]*FYN + APV[IJ]*FYP;

					/* CELL-FACE GRADIENT, VELOCITY AND MASS FLUX */

					DPYN = (P[IJN] - P[IJ]) / DYPN;
					VN = VNL - APVN*VOLN*(DPYN - DPYNL);
					F2[IJ] = D*VN;

					/* COEFFICIENTS OF P' EQUATION, AN(P) AND AS(N) */

					AN[IJ] = -D*APVN*S;
					AS[IJN] = AN[IJ];
				}
			}

			/* BOUNDARY CONDITIONS: PRESCRIBED MASS FLUXES, ZERO CORRECTION
			(EQUIVALENT TO ZERO NORMAL GRADIENT FOR P'; COEFFICIENT FOR
			THE BOUNDARY NODE IS ZERO, NO SPECIAL TREATMENT REQUIRED)*/

			/* SORCE TERM AND COEFFICIENT OF NODE P*/

			SUM = 0.0;

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					SU[IJ] = F1[IJ - NJ] - F1[IJ] + F2[IJ - 1] - F2[IJ];
					AP[IJ] = -(AE[IJ] + AW[IJ] + AN[IJ] + AS[IJ]);
					SUM = SUM + SU[IJ];
					PP[IJ] = 0.0;
				}
			}

			/* SUM MUST BE ZERO IF GLOBAL MASS CONSERVATION IS ASSURED! */

			/* IF(LTEST) WRITE(2,*) '       SUM = ',SUM */

			/* SOLVE EQUATIONS SYSTEM FOR P' AND APPLY CORRECTIONS */

			/*******************************************************************************/

			/****************************************************/

			QueryPerformanceFrequency(&Frequency);
			QueryPerformanceCounter(&StartingTimeP);

			/***************************************************/

			////////////
			// SIPSOL //
			////////////

			/* COEFFICIENTS OF UPPER AND LOWER TRIANGULAR MATRICES */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					LW[IJ] = AW[IJ] / (1.0 + ALFA*UN[IJ - NJ]);
					LS[IJ] = AS[IJ] / (1.0 + ALFA*UE[IJ - 1]);
					P1 = ALFA*LW[IJ]*UN[IJ - NJ];
					P2 = ALFA*LS[IJ]*UE[IJ - 1];
					LPR[IJ] = 1.0 / (AP[IJ] + P1 + P2 - LW[IJ]*UE[IJ - NJ] - LS[IJ]*UN[IJ - 1]);
					UN[IJ] = (AN[IJ] - P1)*LPR[IJ];
					UE[IJ] = (AE[IJ] - P2)*LPR[IJ];
				}
			}

			/* INNER ITERATIONS LOOP*/

			for (int L = 0; L < MAXITP; ++L)
			{
				RESL = 0.0;

				/* CALCULATE RESIDUAL AND OVERWRITE IT BY INTERMEDIATE VECTOR */

				for (int I = 1; I < NIM; ++I)
				{
					int Start = (LI[I] + 1);
					int End = (LI[I] + NJM);
#pragma omp parallel for reduction (+:RESL)
					for (int IJ = Start; IJ < End; ++IJ)
					{
						RES[IJ] = SU[IJ] - AN[IJ]*PP[IJ + 1] - AS[IJ]*PP[IJ - 1] -
							AE[IJ]*PP[IJ + NJ] - AW[IJ]*PP[IJ - NJ] - AP[IJ]*PP[IJ];
						double ABS = abs(RES[IJ]);
						RESL += ABS;
					}
				}

				for (int J = 2; J <= (NI + NI - 2); ++J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50) 
					for (int I = Start; I <= End; ++I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = (RES[IJ] - LS[IJ]*RES[IJ - 1] - LW[IJ]*RES[IJ - NJ])*LPR[IJ];
					}
				}


				/* STORE INITIAL RESIDUAL SUM FOR CHECKING CONV. OF OUTER ITER. */

				if (L == 0)
				{
					RESOR = RESL;
				}

				RSM = RESL / (RESOR + SMALL);

				/* BACK SUBSTITUTION AND CORRECTION */

				for (int J = (NI + NI - 2); J >= 2; --J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50)
					for (int I = End; I >= Start; --I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = RES[IJ] - UN[IJ]*RES[IJ + 1] - UE[IJ]*RES[IJ + NJ];
						PP[IJ] = PP[IJ] + RES[IJ];
					}
				}				

				RSMP = RESOR;

				if (RSM < RESMAXP)
				{
					L = MAXITP;
				}
			}

			/*********************************************************************************/

			QueryPerformanceCounter(&EndingTimeP);
			ElapsedMicrosecondsP.QuadPart = EndingTimeP.QuadPart - StartingTimeP.QuadPart;
			ElapsedMicrosecondsP.QuadPart *= 1000000;
			ElapsedMicrosecondsP.QuadPart /= Frequency.QuadPart;
			TIMEP += ElapsedMicrosecondsP.QuadPart;

			/*********************************************************************************/

			/*******************************************************************************/

			/* CALCULATE PRESSURE CORRECTION AT BOUNDARIES */

			/*--------------------------------------------------------------------------------*/

			/* PBOUND */

			/* This routine calculates boundary values of pressure or
			pressure-correction by extrapolating (linearly) from inside.*/

			/* SOUTH AND NORTH BOUNDARIES */

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I];
				PP[IJ] = PP[IJ + 1] + (PP[IJ + 1] - PP[IJ + 2])*FY[1];
				IJ = LI[I] + NJ - 1;
				PP[IJ] = PP[IJ - 1] + (PP[IJ - 1] - PP[IJ - 2])*(1.0 - FY[NJM - 1 - 1]);
			}

			/* WEST AND EAST BOUNDARIES */

			NJ2 = 2 * NJ;

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[0] + J;
				PP[IJ] = PP[IJ + NJ] + (PP[IJ + NJ] - PP[IJ + NJ2])*FX[1];
				IJ = LI[NI - 1] + J;
				PP[IJ] = PP[IJ - NJ] + (PP[IJ - NJ] - PP[IJ - NJ2])*(1.0 - FX[NIM - 1 - 1]);
			}

			/*--------------------------------------------------------------------------------*/

			/* VALUE OF P' AT REFERENCE LOCATION TO BE SUBTRACTED FROM ALL P' */

			IJPREF = LI[IPR] + JPR;
			PPO = PP[IJPREF];

			/* CORRECT EAST MASS FLUXES  */

			for (int I = 1; I < NIM - 1; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					F1[IJ] = F1[IJ] + AE[IJ]*(PP[IJ + NJ] - PP[IJ]);
				}
			}

			/* CORRECT NORTH MASS FLUXES */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NIM - 1); ++IJ)
				{
					F2[IJ] = F2[IJ] + AN[IJ]*(PP[IJ + 1] - PP[IJ]);
				}
			}

			/* CORRECT PRESSURE AND VELOCITIES AT CELL CENTER */

			for (int I = 1; I < NIM; ++I)
			{
				DX = X[I] - X[I - 1];

				for (int J = 1; J < NJM; ++J)
				{
					int IJ = LI[I] + J;
					RP = 0.5*(R[J] + R[J - 1]);
					DY = Y[J] - Y[J - 1];

					PPE = PP[IJ + NJ]*FX[I] + PP[IJ]*(1.0 - FX[I]);
					PPW = PP[IJ]*FX[I - 1] + PP[IJ - NJ]*(1.0 - FX[I - 1]);
					PPN = PP[IJ + 1]*FY[J] + PP[IJ]*(1.0 - FY[J]);
					PPS = PP[IJ]*FY[J - 1] + PP[IJ - 1]*(1.0 - FY[J - 1]);

					U[IJ] = U[IJ] - (PPE - PPW)*DY*RP*APU[IJ];
					V[IJ] = V[IJ] - (PPN - PPS)*DX*RP*APV[IJ];
					P[IJ] = P[IJ] + URFP*(PP[IJ] - PPO);
				}
			}


			///////////
			// CALCT //
			///////////



			/* This routine solves the temperature equation. Constant
			viscosity, density and Prandtl number are assumed - only
			the density variation due to buoyancy is considered
			using Boussinesq approximation (valid for temperature
			differences less than 5 deg. in water and 20 deg. in air).
			PRR is the reciprocal value of the Prandtl number, 1/Pr.*/

			/* INITIALIZATION OF TEMPORARILY STORED VARIABLES */

			for (int IJ = 0; IJ < NIJ; ++IJ)
			{
				SU[IJ] = 0.0;
				AP[IJ] = 0.0;
			}

			URFTR = 1.0 / URFT;

			/* FLUXES THROUGH INTERNAL EAST CV-FACES */

			for (int I = 1; I < NIM - 1; ++I)
			{
				/* INTERPOLATION FACTORS, DISTANCE FROM P TO E (SAME FOR ALL J)*/

				FXE = FX[I];
				FXP = 1.0 - FXE;
				DXPE = XC[I + 1] - XC[I];

				for (int J = 1; J < NJM; ++J)
				{
					int IJ = LI[I] + J;
					int	IJE = IJ + NJ;

					/* CELL FACE AREA S = DY*RE*1 */

					S = (Y[J] - Y[J - 1])*(R[J] + R[J - 1])*0.5;

					/* COEFFICIENT RESULTING FROM DIFFUSIVE FLUX */

					D = VIS*PRR*S / DXPE;

					/* EXPLICIT CONVECTIVE FLUX FOR UDS AND CDS */

					CE = min(F1[IJ], 0.0);
					CP = max(F1[IJ], 0.0);

					FUDS = CP*T[IJ] + CE*T[IJE];
					FCDS = F1[IJ]*(T[IJE]*FXE + T[IJ]*FXP);

					/* COEFFICIENTS AE(P) AND AW(E) DUE TO UDS */

					AE[IJ] = CE - D;
					AW[IJE] = -CP - D;

					/* SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION */

					SU[IJ] = SU[IJ] + GDST*(FUDS - FCDS);
					SU[IJE] = SU[IJE] - GDST*(FUDS - FCDS);
				}
			}

			/* FLUXES THROUGH INTERNAL NORTH CV FACES */

			for (int J = 1; J < NJM - 1; ++J)
			{
				/* INTERPOLATION FACTORS, DISTANCE FROM P TO N (SAME FOR ALL J) */

				FYN = FY[J];
				FYP = 1.0 - FYN;
				DYPN = YC[J + 1] - YC[J];

				for (int I = 1; I < NIM; ++I)
				{
					int IJ = LI[I] + J;
					int	IJN = IJ + 1;

					/* CELL FACE AREA S = DX*RN*1 */

					S = (X[I] - X[I - 1])*R[J];

					/* COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V) */

					D = VIS*PRR*S / DYPN;

					/* EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS */

					CN = min(F2[IJ], 0.);
					CP = max(F2[IJ], 0.);

					FUDS = CP*T[IJ] + CN*T[IJN];
					FCDS = F2[IJ]*(T[IJN]*FYN + T[IJ]*FYP);

					/* COEFFICIENTS AE(P) AND AW(E) DUE TO UDS */

					AN[IJ] = CN - D;
					AS[IJN] = -CP - D;

					/* SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION */

					SU[IJ] = SU[IJ] + GDST*(FUDS - FCDS);
					SU[IJN] = SU[IJN] - GDST*(FUDS - FCDS);
				}
			}

			/* VOLUME INTEGRALS (SOURCE TERMS) */

			for (int I = 1; I < NIM; ++I)
			{
				DX = X[I] - X[I - 1];

				for (int J = 1; J < NJM; ++J)
				{
					int IJ = LI[I] + J;
					DY = Y[J] - Y[J - 1];
					RP = 0.5*(R[J] + R[J - 1]);
					VOL = DX*DY*RP;

					/* UNSTEADY TERM CONTRIBUTION TO AP AND SU */

					if (LTIME)
					{
						APT = DEN*VOL*DTR;
						SU[IJ] = SU[IJ] + (1. + GAMT)*APT*TO[IJ] - 0.5*GAMT*APT*TOO[IJ];
						AP[IJ] = AP[IJ] + (1. + 0.5*GAMT)*APT;
					}
				}
			}

			/*--------------------------------------------------------------------------------*/

			/* BCT */

			/*  In this routine, boundary conditions for the temperature
			equation are implemented, i.e. heat fluxes through the
			boundary cell faces are calculated. Here, specified wall
			temperature and adiabatic wall (zero heat flux) are considered;
			treatment at symmetry planes is the same as for an adiabatic
			wall, but inlet and outlet require different treatment*/

			/* SOUTH BOUNDARY (ADIABATIC WALL, DT/DY=0, ZERO FLUX) */

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I];
				T[IJ] = T[IJ + 1];
			}

			/* NORTH BOUNDARY (ADIABATIC WALL, DT/DY=0, ZERO FLUX) */

			for (int I = 1; I < NIM; ++I)
			{
				int IJ = LI[I] + NJ - 1;
				T[IJ] = T[IJ - 1];
			}

			/* WEST BOUNDARY (ISOTHERMAL WALL, NON-ZERO DIFFUSIVE FLUX) */

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[1] + J;
				D = 0.5*VIS*PRR*(Y[J] - Y[J - 1])*(R[J] + R[J - 1]) / (XC[1] - XC[0]);
				AP[IJ] = AP[IJ] + D;
				SU[IJ] = SU[IJ] + D*T[IJ - NJ];
			}


			/* EAST BOUNDARY (ISOTHERMAL WALL) */

			for (int J = 1; J < NJM; ++J)
			{
				int IJ = LI[NIM - 1] + J;
				D = 0.5*VIS*PRR*(Y[J] - Y[J - 1])*(R[J] + R[J - 1]) / (XC[NI - 1] - XC[NIM - 1]);
				AP[IJ] = AP[IJ] + D;
				SU[IJ] = SU[IJ] + D*T[IJ + NJ];
			}

			/*--------------------------------------------------------------------------------*/

			/* UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR TEMPERATURE */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					AP[IJ] = (AP[IJ] - AW[IJ] - AE[IJ] - AN[IJ] - AS[IJ])*URFTR;
					SU[IJ] = SU[IJ] + (1.0 - URFT)*AP[IJ]*T[IJ];
				}
			}

			/*******************************************************************************/

			/****************************************************/

			QueryPerformanceFrequency(&Frequency);
			QueryPerformanceCounter(&StartingTimeT);

			/***************************************************/

			////////////
			// SIPSOL //
			////////////

			/* COEFFICIENTS OF UPPER AND LOWER TRIANGULAR MATRICES */

			for (int I = 1; I < NIM; ++I)
			{
				for (int IJ = (LI[I] + 1); IJ < (LI[I] + NJM); ++IJ)
				{
					LW[IJ] = AW[IJ] / (1.0 + ALFA*UN[IJ - NJ]);
					LS[IJ] = AS[IJ] / (1.0 + ALFA*UE[IJ - 1]);
					P1 = ALFA*LW[IJ]*UN[IJ - NJ];
					P2 = ALFA*LS[IJ]*UE[IJ - 1];
					LPR[IJ] = 1.0 / (AP[IJ] + P1 + P2 - LW[IJ]*UE[IJ - NJ] - LS[IJ]*UN[IJ - 1]);
					UN[IJ] = (AN[IJ] - P1)*LPR[IJ];
					UE[IJ] = (AE[IJ] - P2)*LPR[IJ];
				}
			}

			/* INNER ITERATIONS LOOP*/

			for (int L = 0; L < MAXITT; ++L)
			{
				RESL = 0.0;

				/* CALCULATE RESIDUAL AND OVERWRITE IT BY INTERMEDIATE VECTOR */

				for (int I = 1; I < NIM; ++I)
				{
					int Start = (LI[I] + 1);
					int End = (LI[I] + NJM);
#pragma omp parallel for reduction (+:RESL)
					for (int IJ = Start; IJ < End; ++IJ)
					{
						RES[IJ] = SU[IJ] - AN[IJ]*T[IJ + 1] - AS[IJ]*T[IJ - 1] -
							AE[IJ]*T[IJ + NJ] - AW[IJ]*T[IJ - NJ] - AP[IJ]*T[IJ];
						double ABS = abs(RES[IJ]);
						RESL += ABS;
					}
				}

				for (int J = 2; J <= (NI + NI - 2); ++J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50) 
					for (int I = Start; I <= End; ++I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = (RES[IJ] - LS[IJ]*RES[IJ - 1] - LW[IJ]*RES[IJ - NJ])*LPR[IJ];
					}
				}


				/* STORE INITIAL RESIDUAL SUM FOR CHECKING CONV. OF OUTER ITER. */

				if (L == 0)
				{
					RESOR = RESL;
				}

				RSM = RESL / (RESOR + SMALL);

				/* BACK SUBSTITUTION AND CORRECTION */

				for (int J = (NI + NI - 2); J >= 2; --J)
				{
					int Start = max(1, J - NI + 2);
					int End = min(NI - 2, J - 1);

#pragma omp parallel for if ((End-Start)>50)
					for (int I = End; I >= Start; --I)
					{
						int IJ = (I*NI) + (J - I);
						RES[IJ] = RES[IJ] - UN[IJ]*RES[IJ + 1] - UE[IJ]*RES[IJ + NJ];
						T[IJ] = T[IJ] + RES[IJ];
					}
				}

				

				RSMT = RESOR;

				if (RSM < RESMAXT)
				{
					L = MAXITT;
				}

			}

			/*******************************************************************************/

			/*********************************************************************************/

			QueryPerformanceCounter(&EndingTimeT);
			ElapsedMicrosecondsT.QuadPart = EndingTimeT.QuadPart - StartingTimeT.QuadPart;
			ElapsedMicrosecondsT.QuadPart *= 1000000;
			ElapsedMicrosecondsT.QuadPart /= Frequency.QuadPart;
			TIMET += ElapsedMicrosecondsT.QuadPart;

			/*********************************************************************************/

			/////////////////////////////////////////////////////////////////////////////////////////////////////////



			outputfile << setw(5) << fixed << setprecision(3) << right << setfill('0') << ITER + 1 << "  |  "
				<< setw(5) << scientific << setprecision(3) << left << setfill('0') << RSMU << "  |  "
				<< setw(5) << scientific << setprecision(3) << left << setfill('0') << RSMV << "  |  "
				<< setw(5) << scientific << setprecision(3) << left << setfill('0') << RSMP << "  |  "
				<< setw(5) << scientific << setprecision(3) << left << setfill('0') << RSMT << "  ||  "
				<< setw(8) << scientific << setprecision(6) << left << setfill('0') << U[IJMON] << "  |  "
				<< setw(8) << scientific << setprecision(6) << left << setfill('0') << V[IJMON] << "  |  "
				<< setw(8) << scientific << setprecision(6) << left << setfill('0') << P[IJMON] << "  |  "
				<< setw(8) << scientific << setprecision(6) << left << setfill('0') << T[IJMON] << endl;


			////////////////////////////////////////////////////////////////////////////////////////////////////////

			SOURCE = max(max(RSMU, RSMV), max(RSMP, RSMT));

			if (SOURCE < RESMAX)
			{
				outputfile << endl << "Outer Iteration Converged after: " << ITER + 1 << " Iterations" << endl;
				ITER = MAXIT;
			}

			else if (SOURCE > SLARGE)
			{
				outputfile << endl << "Outer Iteration Diverged after: " << ITER + 1 << " Iterations" << endl;
				ITER = MAXIT;
			}

			/////////////////////////////////////////////////////////////////////////////////////////////////////

			/* Progress Bar*/
			int barWidth = 70;
			std::cout << "[";
			int pos = barWidth * (progress / MAXIT);
			for (int i = 0; i < barWidth; ++i)
			{
				if (i < pos) std::cout << "=";
				else if (i == pos) std::cout << ">";
				else std::cout << " ";
			}
			std::cout << "] " << int((progress * 100.0) / MAXIT) << " %\r";
			std::cout.flush();
			progress = ITER;

			///////////////////////////////////////////////////////////////////////////////////////////////////

			if (InterSave == "True" || InterSave == "true")
			{
				if ((ITER%InterSaveNo) == 0 && ITER != 0)
				{
					Intermediate.open(string(Ofilename + ".inter").c_str(), ios::out);
					Intermediate.seekp(0, Intermediate.beg);
					Intermediate << ITER << endl;
					for (int I = 1; I < NI; ++I)
					{
						LI[I] = I*NJ;
						for (int J = 1; J < NJ; ++J)
						{
							int IJ = LI[I] + J;
							Intermediate << setw(5) << fixed << setprecision(3) << right << setfill('0') << I << "     "
								<< setw(5) << fixed << setprecision(3) << right << setfill('0') << J << "     "
								<< setw(18) << fixed << setprecision(12) << left << setfill('0') << U[IJ] << "     "
								<< setw(18) << fixed << setprecision(12) << left << setfill('0') << V[IJ] << "     "
								<< setw(18) << fixed << setprecision(12) << left << setfill('0') << P[IJ] << "     "
								<< setw(18) << fixed << setprecision(12) << left << setfill('0') << T[IJ] << endl;
						}
					}
					Intermediate.close();
				}

			}

		}
	}

	/*****************************************************************************************************/

	QueryPerformanceCounter(&EndingTimeALL);
	ElapsedMicrosecondsALL.QuadPart = EndingTimeALL.QuadPart - StartingTimeALL.QuadPart;
	ElapsedMicrosecondsALL.QuadPart *= 1000000;
	ElapsedMicrosecondsALL.QuadPart /= Frequency.QuadPart;
	double Time = ElapsedMicrosecondsALL.QuadPart;

	/****************************************************************************************************/


	//////////////////////////////////////////////////////////////////////////////////////////////////////

	outputfile << endl << endl << "Elapsed Time solving U-Equation : "
		<< setw(10) << left << fixed << setprecision(6) << TIMEU / 1000000 << " Sec." << endl;
	outputfile << "Elapsed Time solving V-Equation : "
		<< setw(10) << left << fixed << setprecision(6) << TIMEV / 1000000 << " Sec." << endl;
	outputfile << "Elapsed Time solving P-Equation : "
		<< setw(10) << left << fixed << setprecision(6) << TIMEP / 1000000 << " Sec." << endl;
	outputfile << "Elapsed Time solving T-Equation : "
		<< setw(10) << left << fixed << setprecision(6) << TIMET / 1000000 << " Sec." << endl;

	outputfile << endl << endl;

	outputfile << "Percentage of Elapsed Time solving U-Equation : "
		<< setw(5) << left << fixed << setprecision(3) << (TIMEU / Time) * 100 << " % " << endl;
	outputfile << "Percentage of Elapsed Time solving V-Equation : "
		<< setw(5) << left << fixed << setprecision(3) << (TIMEV / Time) * 100 << " % " << endl;
	outputfile << "Percentage of Elapsed Time solving P-Equation : "
		<< setw(5) << left << fixed << setprecision(3) << (TIMEP / Time) * 100 << " % " << endl;
	outputfile << "Percentage of Elapsed Time solving T-Equation : "
		<< setw(5) << left << fixed << setprecision(3) << (TIMET / Time) * 100 << " % " << endl;

	outputfile << endl << endl;

	outputfile << "Percentage of Total Elapsed Time solving The Equations : "
		<< setw(5) << left << fixed << setprecision(3) << ((TIMEU + TIMEV + TIMEP + TIMET) / Time) * 100 << " % " << endl;

	outputfile << endl << endl;

	outputfile << "Total Elapsed Time : "
		<< setw(10) << left << fixed << setprecision(6) << Time / 1000000 << " Sec." << endl;

	//////////////////////////////////////////////////////////////////////////////////////////////////////

	///////////////////////////////////////////////////////////////////////////////////////////////////////////

	if (DAT == "True" || DAT == "true")
	{
		resultfile << "TITLE = \"PREDICTION OF FLOW IN LID DRIVEN CAVITIES\" " << endl;
		resultfile << "VARIABLES = \"X\", \"Y\", \"U\", \"V\", \"Press\", \"Temp\"" << endl;
		resultfile << "ZONE T=\"Cavity\"" << endl;
		resultfile << "I=" << NIM << ", " << "J=" << NJM << endl;
		resultfile << "ZONETYPE=Ordered" << endl;
		resultfile << "DATAPACKING=POINT" << endl;
		resultfile << "DT=(LONGINT LONGINT DOUBLE DOUBLE DOUBLE DOUBLE )" << endl << endl;

		for (int I = 1; I < NI; ++I)
		{
			LI[I] = I*NJ;
			for (int J = 1; J < NJ; ++J)
			{
				int IJ = LI[I] + J;
				resultfile << setw(5) << fixed << setprecision(3) << right << setfill('0') << I << "     "
					<< setw(5) << fixed << setprecision(3) << right << setfill('0') << J << "     "
					<< setw(18) << fixed << setprecision(12) << left << setfill('0') << U[IJ] << "     "
					<< setw(18) << fixed << setprecision(12) << left << setfill('0') << V[IJ] << "     "
					<< setw(18) << fixed << setprecision(12) << left << setfill('0') << P[IJ] << "     "
					<< setw(18) << fixed << setprecision(12) << left << setfill('0') << T[IJ] << endl;
			}
		}
	}


	int IPSIMAX, JPSIMAX, IPSIMIN, JPSIMIN;

	/* STREAMFUNCTION VALUES AT CV-VERTICES (ZERO AT SOUTH-WEST CORNER) */

	PP[LI[0] + 1] = 0.0;

	/* WEST BOUNDARY (APPLICABLE FOR INLET OR OUTLET) */

	for (int J = 1; J < NJM; ++J)
	{
		int IJ = LI[0] + J;
		PP[IJ] = PP[IJ - 1] - F1[IJ];
	}

	/* SOUTH BOUNDARY (APPLICABLE FOR INLET OR OUTLET) */

	for (int I = 1; I < NIM; ++I)
	{
		int IJ = LI[I] + 1;
		PP[IJ] = PP[IJ] - F2[IJ];

		/* Inner Region */
		for (int J = 1; J < NJM; ++J)
		{
			int IJ = LI[I] + J;
			PP[IJ] = PP[IJ - 1] - F1[IJ];
		}
	}

	/* STRENGTH OF PRIMARY AND SECONDARY EDDY (MIN and MAX values) */

	double UIJ, VIJ, Unorth, Usouth, Veast, Vwest;
	double OmegaMAX, OmegaMIN, DUDY, DVDX;
	double PRIPSI, PRIOMEGA, PRII, PRIJ;
	double BR1PSI, BR1OMEGA, BR1I, BR1J;
	double BR2PSI, BR2OMEGA, BR2I, BR2J;
	double BR3PSI, BR3OMEGA, BR3I, BR3J;
	double BL1PSI, BL1OMEGA, BL1I, BL1J;
	double BL2PSI, BL2OMEGA, BL2I, BL2J;
	double BL3PSI, BL3OMEGA, BL3I, BL3J;
	double TL1PSI, TL1OMEGA, TL1I, TL1J;
	double TL2PSI, TL2OMEGA, TL2I, TL2J;

	double PSIMAX = PRIPSI = BR2PSI = BL1PSI = TL1PSI = SMALL;
	double PSIMIN = BR1PSI = BR3PSI = BL2PSI = TL2PSI = LARGE;

	/* Search for Psi, Omega Over the entire domain "Primary, and BR1" */

	for (int I = 0; I < NIM; ++I)
	{
		for (int J = 0; J < NJM; ++J)
		{
			int IJ = LI[I] + J;

			if (PP[IJ] < BR1PSI)
			{
				BR1PSI = PP[IJ];
				BR1I = I;
				BR1J = J;
				Unorth = U[IJ + 1];
				Usouth = U[IJ - 1];
				Veast = V[LI[I + 1] + J];
				Vwest = V[LI[I - 1] + J];
				DUDY = (Unorth - Usouth) * 0.5 * M;
				DVDX = (Veast - Vwest) * 0.5 * N;
				BR1OMEGA = DVDX - DUDY;
			}

			if (PP[IJ] > PRIPSI)
			{
				PRIPSI = PP[IJ];
				PRII = I;
				PRIJ = J;
				Unorth = U[IJ + 1];
				Usouth = U[IJ - 1];
				Veast = V[LI[I + 1] + J];
				Vwest = V[LI[I - 1] + J];
				DUDY = (Unorth - Usouth)*0.5*M;
				DVDX = (Veast - Vwest)*0.5*N;
				PRIOMEGA = DVDX - DUDY;
			}
		}
	}

	outputfile << endl << endl;
	outputfile << "Strenght of The Primary Vortex : "
		<< setw(10) << left << fixed << setprecision(6) << PRIPSI << endl;
	outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << PRII*DY << endl;
	outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << PRIJ*DX << endl;
	outputfile << "OmegaMax: " << setw(8) << left << fixed << setprecision(6) << PRIOMEGA << endl;
	outputfile << endl << endl;

	//outputfile << "Strenght of The BR1 Vortex : "
	//	<< setw(10) << left << fixed << setprecision(6) << BR1PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << BR1I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << BR1J*DX << endl;
	//outputfile << "OmegaMin: " << setw(8) << left << fixed << setprecision(6) << BR1OMEGA << endl;
	//outputfile << endl << endl;

	/*BR2PSI = BL1PSI = TL1PSI = SMALL;
	BR3PSI = BL2PSI = TL2PSI = LARGE;*/

	///* Search for Psi, Omega Over the Bottom Right corner of the domain "BR2, and BR3" */

	//for (int I = (NIM * 0.9); I < NIM; ++I)
	//{
	//	for (int J = 0; J < (NJM * 0.15); ++J)
	//	{
	//		int IJ = LI[I] + J;

	//		if (PP[IJ] < BR3PSI)
	//		{
	//			BR3PSI = PP[IJ];
	//			BR3I = I;
	//			BR3J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth) * 0.5 * M;
	//			DVDX = (Veast - Vwest) * 0.5 * N;
	//			BR3OMEGA = DVDX - DUDY;
	//		}

	//		if (PP[IJ] > BR2PSI)
	//		{
	//			BR2PSI = PP[IJ];
	//			BR2I = I;
	//			BR2J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth)*0.5*M;
	//			DVDX = (Veast - Vwest)*0.5*N;
	//			BR2OMEGA = DVDX - DUDY;
	//		}
	//	}
	//}

	//outputfile << "Strenght of The BR2 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << BR2PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << BR2I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << BR2J*DX << endl;
	//outputfile << "BR2OMEGA: " << setw(12) << left << scientific << setprecision(6) << BR2OMEGA << endl;
	//outputfile << endl << endl;

	//outputfile << "Strenght of The BR3 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << BR3PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << BR3I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << BR3J*DX << endl;
	//outputfile << "BR3OMEGA: " << setw(12) << left << scientific << setprecision(6) << BR3OMEGA << endl;
	//outputfile << endl << endl;

	//BL1PSI = TL1PSI = SMALL;
	//BL2PSI = TL2PSI = LARGE;

	///* Search for Psi, Omega Over the Bottom lef corner of the domain "BL1, and BL2" */

	//for (int I = 0; I < (NIM * 0.15); ++I)
	//{
	//	for (int J = 0; J < (NJM * 0.15); ++J)
	//	{
	//		int IJ = LI[I] + J;

	//		if (PP[IJ] < BL2PSI)
	//		{
	//			BL2PSI = PP[IJ];
	//			BL2I = I;
	//			BL2J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth) * 0.5 * M;
	//			DVDX = (Veast - Vwest) * 0.5 * N;
	//			BL2OMEGA = DVDX - DUDY;
	//		}

	//		if (PP[IJ] > BL1PSI)
	//		{
	//			BL1PSI = PP[IJ];
	//			BL1I = I;
	//			BL1J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth)*0.5*M;
	//			DVDX = (Veast - Vwest)*0.5*N;
	//			BL1OMEGA = DVDX - DUDY;
	//		}
	//	}
	//}

	//outputfile << "Strenght of The BL1 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << BL1PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << BL1I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << BL1J*DX << endl;
	//outputfile << "BL1OMEGA: " << setw(10) << left << fixed << setprecision(6) << BL1OMEGA << endl;
	//outputfile << endl << endl;

	//outputfile << "Strenght of The BL2 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << BL2PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << BL2I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << BL2J*DX << endl;
	//outputfile << "BL2OMEGA: " << setw(12) << left << scientific << setprecision(6) << BL2OMEGA << endl;
	//outputfile << endl << endl;

	//TL1PSI = SMALL;
	//TL2PSI = LARGE;

	///* Search for Psi, Omega Over the Bottom lef corner of the domain "TL1, and TL2" */

	//for (int I = 0; I < (NIM * 0.15); ++I)
	//{
	//	for (int J = (NJM * 0.9); J < NJM; ++J)
	//	{
	//		int IJ = LI[I] + J;

	//		if (PP[IJ] < TL2PSI)
	//		{
	//			TL2PSI = PP[IJ];
	//			TL2I = I;
	//			TL2J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth) * 0.5 * M;
	//			DVDX = (Veast - Vwest) * 0.5 * N;
	//			TL2OMEGA = DVDX - DUDY;
	//		}

	//		if (PP[IJ] > TL1PSI)
	//		{
	//			TL1PSI = PP[IJ];
	//			TL1I = I;
	//			TL1J = J;
	//			Unorth = U[IJ + 1];
	//			Usouth = U[IJ - 1];
	//			Veast = V[LI[I + 1] + J];
	//			Vwest = V[LI[I - 1] + J];
	//			DUDY = (Unorth - Usouth)*0.5*M;
	//			DVDX = (Veast - Vwest)*0.5*N;
	//			TL1OMEGA = DVDX - DUDY;
	//		}
	//	}
	//}

	//outputfile << "Strenght of The TL1 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << TL1PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << TL1I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << TL1J*DX << endl;
	//outputfile << "TL1OMEGA: " << setw(10) << left << fixed << setprecision(6) << TL1OMEGA << endl;
	//outputfile << endl << endl;

	//outputfile << "Strenght of The TL2 Vortex : "
	//	<< setw(12) << left << scientific << setprecision(6) << TL2PSI << endl;
	//outputfile << "Location X: " << setw(8) << left << fixed << setprecision(6) << TL2I*DY << endl;
	//outputfile << "Location Y: " << setw(8) << left << fixed << setprecision(6) << TL2J*DX << endl;
	//outputfile << "TL2OMEGA: " << setw(12) << left << scientific << setprecision(6) << TL2OMEGA << endl;
	//outputfile << endl << endl;


	/*double *HORVORTICITY, *VERVORTICITY;
	HORVORTICITY = (double*)malloc(NIM*sizeof(double));
	VERVORTICITY = (double*)malloc(NJM*sizeof(double));*/
	

	///* Vorticity Valuse across a Horizontal line passing through the geometric center of the domain */

	//for (int I = 0; I < NIM; ++I)
	//{
	//	int IJ = LI(I) + (NJM / 2);

	//	Unorth = U(IJ + 1);
	//	Usouth = U(IJ - 1);
	//	Veast = V(LI(I + 1) + (NJM / 2));
	//	Vwest = V(LI(I - 1) + (NJM / 2));
	//	DUDY = (Unorth - Usouth) * 0.5 * M;
	//	DVDX = (Veast - Vwest) * 0.5 * N;
	//	HORVORTICITY(I) = DUDY - DVDX;
	//}

	///* Vorticity Valuse across a Vertical line passing through the geometric center of the domain */

	//for (int J = 0; J < NJM; ++J)
	//{
	//	int IJ = LI((NIM / 2)) + J;

	//	Unorth = U(IJ + 1);
	//	Usouth = U(IJ - 1);
	//	Veast = V(LI((NIM / 2) + 1)) + J;
	//	Vwest = V(LI((NIM / 2) - 1)) + J;
	//	DUDY = (Unorth - Usouth) * 0.5 * M;
	//	DVDX = (Veast - Vwest) * 0.5 * N;
	//	VERVORTICITY(J) = DVDX - DUDY;
	//}


	///* Vorticity Valuse across a Horizontal line passing through the geometric center of the domain */

	//outputfile << "Vorticity Valuse across a Horizontal line passing through the geometric center of the domain" << endl;

	//for (int I = 0; I < NIM; ++I)
	//{
	//	int IJ = LI(I) + (NJM / 2);

	//	outputfile << setw(5) << fixed << setprecision(3) << right << setfill('0') << I << "     "
	//		<< setw(18) << fixed << setprecision(12) << left << setfill('0') << HORVORTICITY(I) << "     " << endl << endl;

	//}

	///* Vorticity Valuse across a Vertical line passing through the geometric center of the domain */

	//outputfile << "Vorticity Valuse across a Vertical line passing through the geometric center of the domain" << endl << endl;

	//for (int J = 0; J < NJM; ++J)
	//{
	//	int IJ = LI((NIM / 2)) + J;

	//	outputfile << setw(5) << fixed << setprecision(3) << right << setfill('0') << J << "     "
	//		<< setw(18) << fixed << setprecision(12) << left << setfill('0') << VERVORTICITY(J) << "     " << endl << endl;
	//}

free(X);
free(XC);
free(Y);
free(YC);
free(LI);
free(U);
free(V);
free(P);
free(T);
free(PP);
free(UO);
free(VO);
free(PO);
free(TO);
free(UOO);
free(VOO);
free(POO);
free(TOO);
free(AE);
free(AW);
free(AN);
free(AS);
free(AP);
free(LW);
free(LS);
free(UN);
free(UE);
free(RES);
free(LES);
free(LPR);

	return 0;
}

