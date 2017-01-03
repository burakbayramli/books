/* Change this to reflect the apropriate header file */
// #include "c:\matlabr12\extern\include\mex.h"
// #include "c:\matlabr11\extern\include\mex.h"
#include <math.h>
#include "mex.h"

/*
* vech.c
* This is a helper function and is part of the UCSD_MVGARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/
void vech(double *parameters, double *matrix,int m)
{
	int i, j, step;
	int count=0;
	for (i=0;i<m;i++){
		step=i*m;
		for (j=i;j<m;j++){
			*(parameters+count)=*(matrix+step+j);
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
	matrix = mxGetPr(prhs[0]);


	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows  = mxGetM(prhs[0]);
	ncols  = mxGetN(prhs[0]);

	if(ncols!=mrows)
		mexErrMsgTxt("Square Matrix required.");


	/*  Set the output pointer to the output matrix. */
	outsize = mrows*(mrows+1)/2;
	plhs[0] = mxCreateDoubleMatrix(outsize , 1 , mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	parameters = mxGetPr(plhs[0]);


	/*  Call the C subroutine. */
	vech(parameters, matrix, mrows);

}