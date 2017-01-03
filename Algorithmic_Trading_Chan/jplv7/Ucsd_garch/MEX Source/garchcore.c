/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"

/*
* garchcore.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/
void makeh(double *data, double *parameters, int p, int q, int m, int T, double *covEst, double *Ht)
{
	int i, j;
	for (j=0; j<m; j++) {
		*(Ht+j)=*covEst;
	}
	for (i=m; i<T; i++) {
		*(Ht+i)=parameters[0];
		for (j=0; j<p; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+1]* pow(*(data+(i-(j+1))),2);
		}
        for (j=0; j<q; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+p+1]* *(Ht+(i-(j+1)));
		}
 	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *parameters, *Ht, *covEst;
	int p, q, m, T;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=7)
		mexErrMsgTxt("Seven inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Get the scalar inputs */
	p = mxGetScalar(prhs[3]);
	q = mxGetScalar(prhs[4]);
	m = mxGetScalar(prhs[5]);
	T = mxGetScalar(prhs[6]);

	/*  Create a pointer to the input matrices . */
	data           = mxGetPr(prhs[0]);
	parameters     = mxGetPr(prhs[1]);
	covEst         = mxGetPr(prhs[2]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	Ht = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	makeh(data,parameters,p, q, m, T, covEst, Ht);

}