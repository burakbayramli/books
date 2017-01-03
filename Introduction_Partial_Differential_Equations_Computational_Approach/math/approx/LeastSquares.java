package ifi.math.approx;

public class LeastSquares
{
  static public void computeLinLS(int num_pts, double[] x, 
				  double[] y, double[] v)
  {
    // Calculate misc. sums used to find least squares approx.

    double sum_x  = 0.0; // sum of all x values
    double sum_y  = 0.0; // sum of all y values
    double sum_xx = 0.0; // sum of x squared
    double sum_xy = 0.0; // sun of all x times y

    int i;
    for(i=0 ; i < num_pts ; i++)
    {
      sum_x  += x[i];
      sum_y  += y[i];
      sum_xx += x[i]*x[i];
      sum_xy += x[i]*y[i];
    }

    // Find a and b in approx y=a*x+b

    v[0] = (num_pts*sum_xy-sum_x*sum_y) / (num_pts*sum_xx-sum_x*sum_x);
    v[1] = (sum_y-v[1]*sum_x)/num_pts;
  }

  // private variables

  double[] x;
  double[] y;
  
}
