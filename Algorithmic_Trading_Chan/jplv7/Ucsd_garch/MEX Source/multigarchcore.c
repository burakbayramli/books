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
void multigarchcore(double *data, double *dataneg, double *parameters, double *nu, double *lambda, double *b, int p, int o, int q, int m, int T, double *covEst, double *Ht)
{
	int i, j;
	for (j=0; j<m; j++) {
		*(Ht+j)=*covEst;
	}

	for (i=m; i<T; i++) {
		*(Ht+i)=parameters[0];
		for (j=0; j<p; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+1]* pow(*(data+(i-(j+1))),*nu);
		}
		for (j=0; j<o; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+p+1]* pow(*(dataneg+(i-(j+1))),*nu);
		}
        for (j=0; j<q; j++) {
			*(Ht+i)=*(Ht+i)+parameters[j+p+o+1]* pow(*(Ht+(i-(j+1))),*lambda);
		}
		*(Ht+i)=pow(*(Ht+i),1 / *lambda);

 	}
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *dataneg, *parameters, *nu, *lambda, *b, *covEst, *Ht;
	int p, q, m, T, o;

	/*  Get the scalar inputs */
	p = (int)mxGetScalar(prhs[6]);
	o = (int)mxGetScalar(prhs[7]);
	q = (int)mxGetScalar(prhs[8]);
	m = (int)mxGetScalar(prhs[9]);
	T = (int)mxGetScalar(prhs[10]);

	/*  Create a pointer to the input matrices . */
	data		= mxGetPr(prhs[0]);
	dataneg     = mxGetPr(prhs[1]);
	parameters	= mxGetPr(prhs[2]);
	nu			= mxGetPr(prhs[3]);
	lambda		= mxGetPr(prhs[4]);
	b			= mxGetPr(prhs[5]);
	covEst		= mxGetPr(prhs[11]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(T,1, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	Ht = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	multigarchcore(data, dataneg, parameters, nu, lambda, b, p, o, q, m, T, covEst, Ht);
}