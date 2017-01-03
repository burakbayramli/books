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
void makeh(double *data, double *omega, double *alpha, double *beta, int m, int T, int k, int dimP, int dimQ, double *covEst, double *Ht)
{
	int i, j, ii, jj, index;
	for (i=0; i<k; i++)
	{
		index=i*T;
		for (j=0; j<m; j++)
		{
		*(Ht+j+index)=*(covEst+i);
		}
	}

	for (i=m; i<T; i++)  // loop over all T
	{
		for (j=0;j<k;j++) // loop over all elements of H
		{
			*(Ht+i+(j*T))=*(omega+j);
		}

		index=0;
		for (ii=0;ii<dimP;ii++) // loop over all dims of P
		{
			for (j=0;j<k;j++) // loop over all elements of H
			{
				for (jj=0;jj<k;jj++)
				{
					*(Ht+i+j*T)+=*(data+(i-1-ii+jj*T))* *(alpha+index);
					index++;
				}
			}
		}
		index=0;
		for (ii=0;ii<dimP;ii++) // loop over all dims of P
		{
			for (j=0;j<k;j++) // loop over all elements of H
			{
				for (jj=0;jj<k;jj++)
				{
					*(Ht+i+j*T)+=*(Ht+(i-1-ii+jj*T)) * *(beta+index);
					index++;
				}
			}
		}
	}
}





/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *data, *omega, *alpha, *beta, *Ht, *covEst;
	int m,k,t,dimP,dimQ;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=10)
		mexErrMsgTxt("Ten inputs required.");
	if(nlhs!=1)
		mexErrMsgTxt("One output required.");

	/*  Get the scalar inputs */
	m = (int)mxGetScalar(prhs[4]);
	k = (int)mxGetScalar(prhs[5]);
	t = (int)mxGetScalar(prhs[6]);
	dimP = (int)mxGetScalar(prhs[7]);
	dimQ = (int)mxGetScalar(prhs[8]);

	/*  Create a pointer to the input matrices . */
	data           = mxGetPr(prhs[0]);
	omega     = mxGetPr(prhs[1]);
	alpha     = mxGetPr(prhs[2]);
	beta     = mxGetPr(prhs[3]);
	covEst         = mxGetPr(prhs[9]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);

	/*  Create a C pointer to a copy of the output matrix. */
	Ht = mxGetPr(plhs[0]);

	/*  Call the C subroutine. */
	makeh(data, omega, alpha, beta, m, mrows, k, dimP, dimQ, covEst, Ht);

}