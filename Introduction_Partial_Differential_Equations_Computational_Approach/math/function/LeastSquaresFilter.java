package ifi.math.function;

import ifi.math.function.DiscreteFunction;
import ifi.math.function.DiscreteFunctionFilter;

/**
 * The FunctionFilter is an interface for objects that are capable of
 * performing filtering operations on objects that implement the
 * <a href="mathtools.PlottableFunction.html" PlottableFunction</a>
 * interface.
 */

public class LeastSquaresFilter implements DiscreteFunctionFilter
{
  /**
   *
   */

  public LeastSquaresFilter()
  {
  }

  public LeastSquaresFilter(int n)
  {
    setBlockSize(n);
  }

  public void setBlockSize(int n)
  {
    block_size = n;
  }

  /**
   * Perform filtering of the input function, and
   * return a new objects that represents the filtered function.
   */

  public void filter(DiscreteFunction in, DiscreteFunction out)
  {
    int no_orig = in.getSize();
    int no_ls = no_orig-block_size+1;

    // Return if less than two points in new curve
    if( no_ls <= 0 ) 
    {
      out.redim(0);
      return;
    }

    out.redim(no_ls);

    // Used to calculate misc. sums used to find least squares approx.
    
    double sum_x  = 0.0; // sum of all x values
    double sum_y  = 0.0; // sum of all y values
    double sum_xx = 0.0; // sum of x squared
    double sum_xy = 0.0; // sun of all x times y

    int i;
    double x, y;

    for(i=0 ; i < no_orig ; i++)
    {
      // Update sums

      if(i>=block_size)
      {
	// Remove one entry from the sums
	x = in.getX(i-block_size);
	y = in.getY(i-block_size);

	sum_x  -= x;
	sum_y  -= y;
	sum_xx -= x*x;
	sum_xy -= x*y;
      }

      // Add one entry to the sums
      x = in.getX(i);
      y = in.getY(i);
      
      sum_x  += x;
      sum_y  += y;
      sum_xx += x*x;
      sum_xy += x*y;
    
      if(i>=block_size-1)
      {
	// Find a and b in approx y=a*x+b

	double a = (block_size*sum_xy-sum_x*sum_y) /
	           (block_size*sum_xx-sum_x*sum_x);
	double b = (sum_y-a*sum_x)/block_size;

	// Calculate values at the end point, and add to least_square
	// function.

	y = a*x+b;

	out.setX(i-block_size+1, x);
	out.setY(i-block_size+1, y);
      }
    }
  }

  // Internal variables

  int block_size = 0;
}
