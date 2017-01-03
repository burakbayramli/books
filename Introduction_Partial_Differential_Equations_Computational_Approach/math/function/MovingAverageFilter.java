package ifi.math.function;

import ifi.math.function.DiscreteFunction;
import ifi.math.function.DiscreteFunctionFilter;

/**
 * The FunctionFilter is an interface for objects that are capable of
 * performing filtering operations on objects that implement the
 * <a href="mathtools.PlottableFunction.html" PlottableFunction</a>
 * interface.
 */

public class MovingAverageFilter implements DiscreteFunctionFilter
{
  /**
   *
   */

  public MovingAverageFilter()
  {
  }

  public MovingAverageFilter(int n)
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
    // Get number of original data, and return if there
    // are fewer than block_size-1.

    int no_orig = in.getSize();

    if(no_orig<=block_size)
    {
      out.redim(0);
      return;
    }

    // Create a picewise linear function
    out.redim(no_orig-block_size+1);

    int i;
    double sum=0.0;

    for(i=0 ; i<block_size ; i++)
      sum += in.getY(i);

    out.setX(0, in.getX(block_size-1));
    out.setY(0, sum/block_size);

    for(i=block_size ; i < no_orig ; i++)
    {
      sum += in.getY(i);
      sum -= in.getY(i-block_size);

      out.setX(i-block_size+1, in.getX(i));
      out.setY(i-block_size+1, sum/block_size);
    }
  }

  // Internal variables

  int block_size = 0;
}
