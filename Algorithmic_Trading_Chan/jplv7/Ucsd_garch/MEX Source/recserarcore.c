/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"
#include <string.h>

/*
* recserarcore.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 1    Date: 12/31/2001
*/
void recserarcore(double *results, double *a, int p1, int n1, int ncols, int mrows, double *resultout){
	int j,k,n, step, smstep;
	for (j=0;j<ncols;j++){ // Go across colums
		step=j*mrows;
		smstep=j*p1;
		for (k=p1; k<mrows; k++){ // GO down a row
			for (n=0;n<p1;n++){
				*(results+step+k)=*(results+step+k)+a[smstep+n]* *(results+step+k-n-1);
			}
		}
	}
	memcpy(resultout,results,ncols*mrows*sizeof(double));
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *results,*a, *resultout;
	int p1, n1;
	int mrows,ncols;

	/*  Check for proper number of arguments. */

	if(nrhs!=4)
		mexErrMsgTxt("Five inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Get the scalar inputs */
	p1  = mxGetScalar(prhs[2]);
	n1  = mxGetScalar(prhs[3]);


	/*  Create a pointer to the input matrices . */
	results    = mxGetPr(prhs[0]);
	a          = mxGetPr(prhs[1]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	resultout = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	recserarcore(results, a, p1, n1, ncols, mrows, resultout);

}