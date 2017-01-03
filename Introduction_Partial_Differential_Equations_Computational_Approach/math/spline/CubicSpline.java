package ifi.math.spline;

import ifi.math.linalg.LinEqSolve;

/**
 * Class for creating cubic splines from discrete data. The spline
 * functions can be made an interpolant or an approximation of the
 * input data.
 */

public class CubicSpline
{
  /**
   * Create a spline object. One of the functions interpolate or
   * approximate should be called in order to create a valid spline.
   */

  public CubicSpline ()
  {
    bs = new double[4];
    ok = false;
  }

  /**
   * Evaluate the function at a specified point. The function
   * must be created by interpolate or approximate first.
   */

  public double value (double x)
  {
    if (!ok) {
      System.out.println ("Call interpolation or approximation first");
      return 0.;
    }

    knot_vec.BSplineValues (x,bs);
    int miu = knot_vec.getMiu ();
    double v = 0.;
    for (int i=0; i<4; i++)
      v += coef[miu-4+i]*bs[i];
    return v;
  }

  /**
   * Create an interpolation to the input points.
   */

  public boolean interpolate (int num_pts, double[] x, double[] f)
  {
    if (num_pts<4)
    {
      System.out.println ("Give at least 4 points!");
      return false;
    }

    double[] knots = new double[num_pts-2];
    knots[0] = x[0]; knots[num_pts-3]=x[num_pts-1];
    int i;
    for (i=1; i<num_pts-3; i++)
      knots[i] = x[i+1];

    if (knot_vec==null)
      knot_vec = new KnotVec (num_pts-2,knots,4);
    else
      knot_vec.redim (num_pts-2,knots,4);

    int dimension = num_pts;
    double[][] A = new double[num_pts][7];
    knot_vec.BSplineValuesTableABand (num_pts,x,A);

    coef = new double[dimension];
    LinEqSolve.factLUBand (dimension,4,A);
    LinEqSolve.forwBackLUBand (dimension,4,A,f,coef);

    ok = true;
    return ok;
  }

  /**
   * Create an approximation to the input points. The smothness
   * of the aproximation is controlled by the parameter "level".
   * A level close to zero will give a very smooth curve, while
   * the level 1.0 gives interpolation.
   */

  public boolean approximate (int num_pts, double[] x,
			      double[] f,
			      double level)
  {
    if (num_pts<4)
    {
      System.out.println ("Give at least 4 points!");
      return false;
    }
    if (num_pts==4)
      return interpolate (num_pts,x,f);

    int num_new_pts = (int) (((double) num_pts-1.0)*level+0.5)+1;
    if (num_new_pts<2)
      num_new_pts = 2;
    if (num_new_pts>num_pts-2)
      num_new_pts = num_pts-2;
    double interval = ((double) num_pts-1.0)/((double) num_new_pts-1.0);

    double[] knots = new double[num_new_pts];
    knots[0] = x[0]; knots[num_new_pts-1] = x[num_pts-1];
    // TVS:    double delta = (x[num_pts-1]-x[0])/(num_new_pts-1);

    int i,j,k;
    for (i=1; i<num_new_pts-1; i++)
    {
      j = (int) (i*interval+0.5);
      knots[i] = x[j];

      // TVS: knots[i] = x[0]+i*delta;
    }

    if (knot_vec==null)
      knot_vec = new KnotVec (num_new_pts,knots,4);
    else
      knot_vec.redim (num_new_pts,knots,4);

    int dimension = knot_vec.getLength()-4;
    double[][] A = new double[num_pts][dimension];
    knot_vec.BSplineValuesTableA (num_pts,x,A);

    double[][] ATA = new double[dimension][dimension];
    coef = new double[dimension];
    double[] rhs = new double[dimension];

    for (i=0; i<dimension; i++)
    {
      rhs[i] = 0.;
      for (k=0; k<num_pts; k++)
	rhs[i] += A[k][i]*f[k];
      for (j=0; j<dimension; j++)
      {
	ATA[i][j] = 0.;
	for (k=0; k<num_pts; k++)
	  ATA[i][j] += A[k][i]*A[k][j];
      }
    }

    LinEqSolve.factLU (dimension,ATA);
    LinEqSolve.forwBackLU (dimension,ATA,rhs,coef);

    ok = true;
    return ok;
  }

  KnotVec knot_vec;
  boolean ok;
  double[] coef;
  double[] bs;
}
