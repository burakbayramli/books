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
void makeh(double *data, double *dataneg, double *parameters, int p, int o, int q, int m, int T, double *covEst, double *Ht)
{
	int i, j;
	for (j=0; j<m; j++) {
		*(Ht+j)=*covEst;
	}
	for (i=m; i<T; i++) {
		*(Ht+i)=parameters[0];
		for (j=0; j<p; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+1]* *(data+(i-(j+1)));
		}
		for (j=0; j<o; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+p+1]* *(dataneg+(i-(j+1)));
		}
        for (j=0; j<q; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+p+o+1]* *(Ht+(i-(j+1)));
		}
 	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *dataneg, *parameters, *Ht, *covEst;
	int p, o, q, m, T;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=9)
		mexErrMsgTxt("Nine inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Get the scalar inputs */
	p = (int)mxGetScalar(prhs[4]);
	o = (int)mxGetScalar(prhs[5]);
	q = (int)mxGetScalar(prhs[6]);
	m = (int)mxGetScalar(prhs[7]);
	T = (int)mxGetScalar(prhs[8]);

	/*  Create a pointer to the input matrices . */
	data           = mxGetPr(prhs[0]);
	dataneg        = mxGetPr(prhs[1]);
	parameters     = mxGetPr(prhs[2]);
	covEst         = mxGetPr(prhs[3]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	Ht = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	makeh(data, dataneg,parameters,p, o, q, m, T, covEst, Ht);

}