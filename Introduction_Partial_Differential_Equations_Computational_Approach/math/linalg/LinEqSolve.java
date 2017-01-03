package ifi.math.linalg;

/**
 * Solve a linear system of equations. LU factorizations for full
 * or banded systems are provided. In order to solve a system one should
 * start by factorizing the system matrix (factLU or factBandLU), and
 * then procede by performing a forward and backward substitution
 * (forwBackLU or forwBackBandLU).
 */

public class LinEqSolve
{
  /**
   * Create an LU factorization of the input matrix. The result will
   * be stored in the input matrix.
   */

  public static void factLU (int dim, double[][] A)
  {
    int i,j,k;
    double sum,h;

    for (j=0; j<dim; j++)
    {
      for (i=0; i<j; i++)
      {
	sum = A[i][j];
	for (k=0; k<i; k++)
	  sum -= A[i][k]*A[k][j];
	A[i][j] = sum;
      }

      for (i=j; i<dim; i++)
      {
	sum = A[i][j];
	for (k=0; k<j; k++)
	  sum -= A[i][k]*A[k][j];
	A[i][j] = sum;
      }

      if (j!=dim-1) {
	if (Math.abs(A[j][j])<1.0e-12)
	  System.out.println ("Matrix factLU, dividing by "
			      +String.valueOf(A[j][j])
			      +" in Gaussian elimination, row="
			      +String.valueOf(j+1));

	h = 1.0/A[j][j];
	for (i=j+1; i<dim; i++)
	  A[i][j] *= h;
      }
    }
  }

  /**
   * Create an LU factorization of the banded input matrix. The result will
   * be stored in the input matrix.
   */

  public static void factLUBand (int dim, int bandwidth, double[][] A)
  {
    int i,j,k,ik,jstop = 2*bandwidth-1;
    double c;
    for (k=0; k<dim; k++) {
      c = A[k][bandwidth-1];
      if (Math.abs(c)<1.0e-12)
	System.out.println ("Matrix factLU, dividing by "
			    +String.valueOf(c)
			    +" in Gaussian elimination, row="
			    +String.valueOf(k+1));
      for (j=bandwidth; j<jstop; j++)
	A[k][j] /= c;

      i = 1;
      while ((bandwidth-i)>0 && ((i+k)<=dim-1)) {
	ik = i+k;
	c = A[ik][bandwidth-i-1];
	for (j=bandwidth; j<jstop; j++)
	  A[ik][j-i] -= A[k][j]*c;
	i++;
      }
    }
  }

  /**
   * Perform a forward backward substitution on the given right hand side
   * (rhs) using the given system matrix. The matrix is assumed to be
   * factorized. The result of the substitution (the solution x of
   * Ax=b) is returned in the vector x.
   */

  public static void forwBackLU (int dim, double[][]A,
				 double[] rhs, double[] x)
  {
    int i,j;
    int ii = -1;
    double sum;

    for (i=0; i<dim; i++)
    {
      sum = rhs[i];
      if (ii>-1)
	for (j=ii; j<=i-1; j++)
	  sum -= A[i][j]*x[j];
      else if (sum!=0.0)
	ii = i;

      x[i] = sum;
    }

    for (i=dim-1; i>=0; i--)
    {
      sum = x[i];
      for (j=i+1; j<dim; j++)
	sum -= A[i][j]*x[j];
      x[i] = sum/A[i][i];
    }
  }

  /**
   * Perform a forward backward substitution on the given right hand side
   * (rhs) using the given banden system matrix. The matrix is assumed to be
   * factorized. The result of the substitution (the solution x of
   * Ax=b) is returned in the vector x.
   */

  public static void forwBackLUBand (int dim, int bandwidth,
				     double[][]A, double[] rhs, double[] x)
  {
    int i,j,k;
    double c;

    for (k=0; k<dim; k++)
      x[k] = rhs[k];

    for (k=0; k<dim; k++)
    {
      x[k] /= A[k][bandwidth-1];
      c = x[k];
      i = 1;

      while ((bandwidth-i)>0 && ((i+k)<=dim-1))
      {
	x[i+k] -= c*A[i+k][bandwidth-i-1];
	i++;
      }
    }

    for (i=dim-2; i>0; i--)
    {
      c = x[i];
      j = 0;
      while ((j<(bandwidth-1)) && ((j+i)<=(dim-2)))
      {
	c -= A[i][j+bandwidth]*x[i+j+1];
	j++;
      }
      x[i] = c;
    }
  }
}
