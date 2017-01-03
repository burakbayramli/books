/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"

/*
* ivech.c
* This is a helper function and is part of the UCSD_MVGARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/
void ivech(double *parameters, double *matrix,int m)
{
	int i, j;
	int count=0;
	for (i=0;i<m;i++){
		for (j=i;j<m;j++){
			*(matrix+i*m+j)=*(parameters+count);
			//*(matrix+i+j*m)=*(parameters+count);
		count++;
		}
	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
	double *parameters, *matrix;
	int mrows,ncols, outsize;

	/*  Check for proper number of arguments. */
	if(nrhs!=1)
		mexErrMsgTxt("One inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Create a pointer to the input matrices . */
	parameters = mxGetPr(prhs[0]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	if(ncols!=1)
		mexErrMsgTxt("Column vector required.");
 	outsize=(1 + (int)sqrt(1+8*mrows))/2 - 1;

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(outsize , outsize, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	matrix = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	ivech(parameters, matrix, outsize);
}