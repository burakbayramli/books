/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"

/*
* garchgrad.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/


void makegrad(double *garchp, int p, int q, double *data, double *h, int m, int T, double *covEst, int mrows, double *d, double *e, double *f)
{
	int i, j, k, diff, pmrows=p*mrows, qmrows=q*mrows;
	double covEst2 = *covEst * *covEst;
	for (j=0; j<m; j++) {
		*(d+j)=1;
		for (k=0;k<pmrows;k+=mrows){
			*(e+j+k)=covEst2;
		}
		for (k=0;k<qmrows;k+=mrows){
			*(f+j+k)=covEst2;
		}
	}
	for (i=m; i<T; i++) {
		// First d
		*(d+i)=1;
		for (j=0;j<q;j++){
			*(d+i)+=*(d+i-1-j) * garchp[j];
		}
		//Then the e's
		for (j=0;j<p;j++){
			diff=mrows*j;
			*(e+i+diff)=pow(*(data+i-1-j),2);
			for (k=0;k<q;k++){
				*(e+i+diff)+=*(e+i+diff-k-1) * garchp[j];
			}
		}
		//Finally the f's
		for (j=0;j<q;j++){
			diff=mrows*j;
			*(f+i+diff)=*(h+i-1-j);
			for (k=0;k<q;k++){
				*(f+i+diff)+=*(f+i+diff-k-1) * garchp[j];
			}
		}
	}
}



/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *garchp, *d, *e, *f, *covEst, *h;
	int p, q, m, T;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=8)
		mexErrMsgTxt("Eight inputs required.");
	if(nlhs!=3)
		mexErrMsgTxt("Three outputs required.");

	/*  Get the scalar inputs */
	p = mxGetScalar(prhs[1]);
	q = mxGetScalar(prhs[2]);
	m = mxGetScalar(prhs[5]);
	T = mxGetScalar(prhs[6]);

	/*  Create a pointer to the input matrices . */
	garchp   = mxGetPr(prhs[0]);
	data     = mxGetPr(prhs[3]);
	h        = mxGetPr(prhs[4]);
	covEst   = mxGetPr(prhs[7]);


	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[3]);
	ncols = mxGetN(prhs[3]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,1, mxREAL);
	plhs[1] = mxCreateDoubleMatrix(mrows,p, mxREAL);
	plhs[2] = mxCreateDoubleMatrix(mrows,q, mxREAL);


	/*  Create a C pointer to a copy of the output matrix. */
	d = mxGetPr(plhs[0]);
	e = mxGetPr(plhs[1]);
	f = mxGetPr(plhs[2]);

	/*  Call the C subroutine. */
	makegrad(garchp, p, q, data, h, m, T, covEst, mrows, d, e, f);
}