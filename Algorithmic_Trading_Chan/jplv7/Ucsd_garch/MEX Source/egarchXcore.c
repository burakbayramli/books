/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"

/*
* egarchcore.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/

void egarchcore(double *data, double *parameters, int p, int o, int q, int m, int T, double *covEst, double *Ht, double *X)
{
	int i, j, p2=2*p;
	const double covEst2= *covEst * *covEst;
	for (j=0; j<m; j++) {
		*(Ht+j)=covEst2;
	}
	for (i=m; i<(T); i++) {
		*(Ht+i)=parameters[0]+*(X+i);
		for (j=0; j<p; j++) {
			*(Ht+i)+=parameters[j+1]* *(data+(i-(j+1)))/ *(Ht+i-(j+1));
		}
		for (j=0; j<o; j++) {
			*(Ht+i)+=parameters[j+p+1]* fabs(*(data+(i-(j+1)))/ *(Ht+i-(j+1)));
		}
        for (j=0; j<q; j++) {
			*(Ht+i)+=parameters[j+p2+1]* log(*(Ht+(i-(j+1))));
		}
		*(Ht+i)= exp(*(Ht+i));
	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *parameters, *Ht, *covEst, *X;
	int p, o, q, m, T;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=9)
		mexErrMsgTxt("Nine inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Get the scalar inputs */
	p = mxGetScalar(prhs[3]);
	o = mxGetScalar(prhs[4]);
	q = mxGetScalar(prhs[5]);
	m = mxGetScalar(prhs[6]);
	T = mxGetScalar(prhs[7]);

	/*  Create a pointer to the input matrices . */
	data           = mxGetPr(prhs[0]);
	parameters     = mxGetPr(prhs[1]);
	covEst         = mxGetPr(prhs[2]);
	X              = mxGetPr(prhs[8]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	Ht = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	egarchcore(data,parameters,p,o, q, m, T, covEst, Ht, X);

}