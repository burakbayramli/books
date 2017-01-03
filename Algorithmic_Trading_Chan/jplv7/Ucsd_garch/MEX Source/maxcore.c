/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"


/*
* maxcore.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/
void maxcore(double *y, double *parameters, int ma, int T, double *errors, double *SSE)
{

	int i, j,count=ma;
	*SSE=0;
	for (j=0; j<ma; j++) {
		*(errors+j)=0;
	}
	for (i=ma; i<(T); i++) {
		*(errors+i)=*(y+i);
		for (j=0; j<ma; j++) {
			*(errors+i)=*(errors+i)-parameters[j]* *(errors+(i-(j+1)));
		}
		*SSE=*SSE+pow(*(errors+i),2);
	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *y, *parameters, *errors, *SSE;
	int ma, T;
	int mrows,ncols;

	/*  Check for proper number of arguments. */
	if (nrhs!=4)
		mexErrMsgTxt("Four inputs required.");
	if (nlhs!=1 && nlhs!=2)
		mexErrMsgTxt("One or Two outputs required.");

	/*  Get the scalar inputs */
	ma = mxGetScalar(prhs[2]);
	T  = mxGetScalar(prhs[3]);

	/*  Create a pointer to the input matrices . */
	y          = mxGetPr(prhs[0]);
	parameters = mxGetPr(prhs[1]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);
	plhs[1] = mxCreateDoubleMatrix(1,1, mxREAL);
	/*  Create a C pointer to a copy of the output matrix. */
	errors = mxGetPr(plhs[0]);
	SSE = mxGetPr(plhs[1]);
	/*  Call the C subroutine. */
	maxcore(y, parameters, ma, T, errors, SSE);
}