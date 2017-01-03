package ifi.math.spline;

/**
 * KnotVec objects contains the knot vectors used by spline function.
 * In addition to holding the actual data, the KnotVec class also
 * provides methods for evaluating the B-spline basis functions defined
 * by the knot vector.
 */

public class KnotVec
{
  /**
   * Create an empty knot vector.
   */

  public KnotVec ()
  {
    length = 0;
    order = 0;
    miu = -1;
  }

  /**
   * Create a knot vector using the specified parameter values.
   * Also set the order of the B-spline basis to k. The generated
   * knot sequence will contain (order-1) equal knots at each
   * endpoint.
   */

  public KnotVec (int num_pts, double[] knots, int k)
  {
    if (k<0) k = 4;
    order = k;
    length = num_pts+2*(order-1);
    kvec = new double[length];
    int i;
    for (i=0; i<order-1; i++)
    {
      kvec[i] = knots[0];
      kvec[length-i-1] = knots[num_pts-1];
    }

    for (i=0; i<num_pts; i++)
      kvec[order+i-1] = knots[i];

    if (order==4)
    {
      mat1 = new double[order-2][order-1];
      mat2 = new double[order-1][order];
    }
  }

  /**
   * Recreate a knot vector using the specified parameter values.
   * Also set the order of the B-spline basis to k. The generated
   * knot sequence will contain (order-1) equal knots at each
   * endpoint.
   */

  public boolean redim (int num_pts, double[] knots, int k)
  {
    if (k<0) k = 4;
    order = k;
    length = num_pts+2*(order-1);
    kvec = new double[length];
    int i;
    for (i=0; i<order-1; i++)
    {
      kvec[i] = knots[0];
      kvec[length-i-1] = knots[num_pts-1];
    }

    for (i=0; i<num_pts; i++)
      kvec[order+i-1] = knots[i];

    if (order==4)
    {
      mat1 = new double[order-2][order-1];
      mat2 = new double[order-1][order];
    }

    return true;
  }

  /**
   * Get the number of knots in the knot vector.
   */

  public int getLength()
  {
    return length;
  }

  /**
   * Get the order of the B-spline basis.
   */

  public int getOrder()
  {
    return order;
  }

  /**
   * Get the current value of Miu.
   */

  public int getMiu()
  {
    return miu;
  }

  /**
   * Calculate the number Miu so that kvec[Miu] <= x <= kvec[Miu+1].
   */

  protected void findMiu (double x)
  {
    int mmax = length, mmin = 1, mid;

    if (x<=kvec[0])
    {
      miu = order;
      return;
    }

    if (x>=kvec[length-1])
    {
      miu = length-order;
      return;
    }

    // Check if current miu is correct.

    if(x>=kvec[miu-1] && x<kvec[miu])
    {
      return;
    }

    // Check if miu+1 is correct

    if(x>=kvec[miu] && x<kvec[miu+1])
    {
      miu++;
      return;
    }

    // Otherwise binary search.

    mid = (int) (mmin+mmax)/2;
    while (mmin<mid)
    {
      if (kvec[mid-1]<x)
	mmin = mid;
      else if (kvec[mid-1]>x)
	mmax = mid;
      else
	mmin = mmax = mid;

      mid = (int) (mmin+mmax)/2;
    }

    miu = mid;
  }

  /**
   * Calculate the values of the B-spline basis functions that
   * may have non-zero values at the specified point. This function is
   * only implemented for k=4.
   */

  public void BSplineValues (double x, double[] bs)
  {
    findMiu (x);
    //System.out.println ("x="+String.valueOf(x)+" miu="+String.valueOf(miu));
    if (order!=4)
    {
      System.out.println ("Not implemented!");
      return;
    }

    double r1,r2,r3,r4,r5;
    r1 = (kvec[miu]-x)/(kvec[miu]-kvec[miu-1]);
    r2 = (x-kvec[miu-1])/(kvec[miu]-kvec[miu-1]);
    int i,j;

    for (i=miu; i<miu+2; i++)
    {
      mat1[i-miu][i-miu] = (kvec[i]-x)/(kvec[i]-kvec[i-2]);
      mat1[i-miu][i-miu+1] = (x-kvec[i-2])/(kvec[i]-kvec[i-2]);
    }

    for (i=miu; i<miu+3; i++)
    {
      mat2[i-miu][i-miu] = (kvec[i]-x)/(kvec[i]-kvec[i-3]);
      mat2[i-miu][i-miu+1] = (x-kvec[i-3])/(kvec[i]-kvec[i-3]);
    }

    r3 = r1*mat1[0][0];
    r4 = r1*mat1[0][1]+r2*mat1[1][1];
    r5 = r2*mat1[1][2];

    bs[0] = r3*mat2[0][0];
    bs[1] = r3*mat2[0][1]+r4*mat2[1][1];
    bs[2] = r4*mat2[1][2]+r5*mat2[2][2];
    bs[3] = r5*mat2[2][3];
  }

  /**
   * Calculate the B-spline basis functions at all the points
   * in the x-array. Store the result in the full matrix A.
   */

  public void BSplineValuesTableA (int num_pts,
				   double[] x, double[][] A)
  {
    double[] bs = new double[order];
    int i,j;
    for (i=0; i<num_pts; i++)
    {
      BSplineValues (x[i],bs);
      for (j=0; j<order; j++)
	A[i][miu-order+j] = bs[j];
    }
  }

  /**
   * Calculate the B-spline basis functions at all the points
   * in the x-array. Store the result in the banded matrix A.
   */

  public void BSplineValuesTableABand (int num_pts, double[] x, double[][] A)
  {
    double bs[] = new double[order];
    int i,j;
    for (i=0; i<num_pts; i++)
    {
      BSplineValues (x[i],bs);
      for (j=0; j<order; j++)
	A[i][miu-i+j-1] = bs[j];
    }
  }

  /**
   * Print the values of the knot vector to System.out.
   */

  public void print (String vec_name)
  {
    if (length==0)
      System.out.println ("Knot vec '"+vec_name+"' is not ok!");
    else
    {
      System.out.println ("Knot vec '"+vec_name+"' has "
			  +String.valueOf(length)+" entries");
      String content = "";
      for (int i=0; i<length; i++)
	content += String.valueOf(kvec[i])+" ";
      System.out.println (content);
    }
  }

  // Data members

  protected int length, order;
  private int miu;
  double[] kvec;
  double[][] mat1, mat2;
}
