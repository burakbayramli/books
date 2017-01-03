/* Change this to reflect the apropriate header file */
#include <math.h>
#include "mex.h"

/*
* gimcore.c -
* This is a helper function and is part of the UCSD_GARCH toolbox
* You can compile it and should work on any platform.
*
* Author: Kevin Sheppard
* kevin.sheppard@economics.ox.ac.uk
* Revision: 2    Date: 12/31/2001
*/
void gimcore(double *regressand, double *he, double *covEst, double *map, double *garchconst, double *garchimp, double *archp,  double *garchp, int m, int ma, int p, int q, int T, double *Et ,double *Ht, double *SSE)
{
	int i, j;
	*SSE=0;
	for (j=0; j<m; j++) {
		*(Ht+j)=*covEst * *covEst;
		*(he+j)=*covEst;
		*(Et+j)=0;
	}
	for (i=m; i<T; i++) {
		//First update H(t)
		*(Ht+i)=*garchconst;
		for (j=0; j<p; j++) {
			*(Ht+i)+=archp[j]* pow(*(he+(i-(j+1))),2);
		}
        for (j=0; j<q; j++) {
			*(Ht+i)+=garchp[j]* *(Ht+(i-(j+1)));
		}
		//Then calculate the error for this period
		*(Et+i)=*(regressand+i)- garchimp[0] * sqrt(*(Ht+i));
		for (j=0;j<ma;j++){
			*(Et+i) -=map[j] * *(Et+i-1-j);
		}
		*(he+i)=*(Et+i);
		*SSE+=*(Et+i) * *(Et+i) / *(Ht+i) ;
    }
}


/* The gateway routine */
void mexFunction( int nlhs, mxArray *plhs[],
				 int nrhs, const mxArray *prhs[])
{
	double *regressand, *he, *covEst, *map, *garchconst, *garchimp, *archp, *garchp, *Ht, *Et, *SSE;
	int m, ma, p, q,  T;
	int     mrows,ncols;

	/*  Check for proper number of arguments. */
	if(nrhs!=13)
		mexErrMsgTxt("Fourteen inputs required.");
	if(nlhs!=1 && nlhs!=2 && nlhs!=3)
		mexErrMsgTxt("One, Two, or Three outputs required.");

	/*  Get the scalar inputs */
	m  = (int)mxGetScalar(prhs[8]);
	ma = (int)mxGetScalar(prhs[9]);
	p  = (int)mxGetScalar(prhs[10]);
	q  = (int)mxGetScalar(prhs[11]);
	T  = (int)mxGetScalar(prhs[12]);

	/*  Create a pointer to the input matrices . */
	regressand	= mxGetPr(prhs[0]);
	he			= mxGetPr(prhs[1]);
	covEst		= mxGetPr(prhs[2]);
	map			= mxGetPr(prhs[3]);
	garchconst  = mxGetPr(prhs[4]);
	garchimp    = mxGetPr(prhs[5]);
	archp       = mxGetPr(prhs[6]);
	garchp      = mxGetPr(prhs[7]);

	/*  Get the dimensions of the matrix input ht to make an output matrix. */
	mrows = mxGetM(prhs[0]);
	ncols = mxGetN(prhs[0]);

	/*  Set the output pointer to the output matrix. */
	plhs[0] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);
	plhs[1] = mxCreateDoubleMatrix(mrows,ncols, mxREAL);
	plhs[2] = mxCreateDoubleMatrix(1,1, mxREAL);


	/*  Create a C pointer to a copy of the output matrix. */
	Et  = mxGetPr(plhs[0]);
	Ht  = mxGetPr(plhs[1]);
	SSE = mxGetPr(plhs[2]);

	/*  Call the C subroutine. */
	gimcore(regressand,he,covEst,map,garchconst,garchimp,archp,garchp,m,ma,p,q,T, Et, Ht, SSE);
}