#include "mex.h"
        
#include<stdio.h>
#include<math.h>

/* The gateway routine */
void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{

/* y = sC(A,x) */
/* A is assumed to be sparse, m by n */
/* x is N by n */
/* y is N by 1 */

  double *x, *y, *temp;
  int mA,nA,Nx,i,j,k;

  /* Set up sparse variables */

  double  *pr, *pi;
  int     *ir, *jc;
  int      col, total;
  int      starting_row_index, stopping_row_index, current_row_index;
  
  /* Create pointer to x */
  x = mxGetPr(prhs[1]);

  mA = mxGetM(prhs[0]);
  nA = mxGetN(prhs[0]);
  Nx = mxGetM(prhs[1]);
  
 /* Get the starting positions of all four sparse data arrays */ 
  pr = mxGetPr(prhs[0]);
  pi = mxGetPi(prhs[0]);
  ir = mxGetIr(prhs[0]);
  jc = mxGetJc(prhs[0]);


  /* Set output pointer to point to y */
  plhs[0] = mxCreateDoubleMatrix(Nx,1, mxREAL);

  /* Create a pointer to a copy of y */
  y = mxGetPr(plhs[0]);

  temp = ((double*)mxCalloc(mA, sizeof(double)));

  for (k=0;k<Nx;k++) {
	  total=0;

	  for (i=0;i<mA;i++)
		  *(temp+i)=0.;

	  for (col=0; col<nA; col++)  { 
		  starting_row_index = jc[col]; 
		  stopping_row_index = jc[col+1]; 
		  if (starting_row_index == stopping_row_index)
			  continue;
		  else {
			  for (current_row_index = starting_row_index; 
				   current_row_index < stopping_row_index; 
				   current_row_index++)  {
				  if ((pr[total++]*(2.*(*(x+k + Nx*col)) - 1.))==1.) {
					  *(temp+ir[current_row_index])=1.;
				  }
			  }
		  }
	  }
	  *(y+k)=0.;
	  for (i=0;i<mA;i++)
		  *(y+k)+=*(temp+i);
  }
  
  mxFree(temp);

}
