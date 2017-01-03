package ifi.math.function;

import ifi.plot.PlotableFunction;
import ifi.plot.BoundingBox;
import java.awt.Color;

/**
 * <em>DiscreteFunction</em> implements a picewise linear function from
 * R1 to R1.
 */

public class DiscreteFunction
implements PlotableFunction
{
  /**
   * Construct a DiscreteFunction without allocating any memory yet.
   */

  public DiscreteFunction()
  {
    size = 0;
  }

  /**
   * Construct a DiscreteFunction function capable of holding n samples.
   */

  public DiscreteFunction(int n)
  {
    redim(n);
  }

  /**
   * Construct a DiscreteFunction function, and initialize it with
   * the samples provided.
   */

  public DiscreteFunction(double[] x, double[] y)
  {
    size=x.length;

    arg=x;
    val=y;
  }
  
  /**
   * Construct a DiscreteFunction function, and initialize it with a Function
   * object. The number of sample points is given as a second argument.
   */

  public DiscreteFunction ( Function f, int n )
  {
    redim(n);
    function = f;
    sample();
  }

  /** 
   * Return the size of this function
   */

  public int getSize()
  {
    return size;
  }

  /**
   * Allocate new vectors
   */

  public void redim(int n)
  {
    if( n<=0 )
    {
      size=0;
      arg=null;
      val=null;
    }
    else if( size != n)
    {
      size=n;
      arg=new double[n];
      val=new double[n];
    }
  }
  
  /**
   * Sample the values of the discrete function from the attached Function
   * object.
   */
  
  public void sample ()
  {
    double l = function.getLeft();
    double r = function.getRight();
    double dx = ( r - l ) / ( size - 1.0);
    for ( int i = 0; i < size; i++ )
    {
      arg[i] = l + i*dx;
      val[i] = function.value(arg[i]);
    }
  }
  
  /**
   * This function resamples the values of the discrete function. It's primary
   * use is for animations, where some state in the attached function change,
   * but not the number of sampling points or the left and right boundary.
   */
  
  public void reSample()
  {
    for ( int i = 0 ; i < size; i++ )
    {
      val[i] = function.value(arg[i]);
    }
  }

  /**
   * Set the number of sampling points for this function.
   */

  public void setNumSamplingPoints( int n )
  {
    size = n;
  }

  /**
   * Return the number of sampling points that this function consists
   * of.
   */

  public int numSamplingPoints()
  {
    return size;
  }

  /**
   * Returns the x and y value of sample i. The samples are numbered 
   * starting from zero to numSamplingPoints-1.
   */

  public void getSample(int i, double[] point)
  {
    point[0] = arg[i];
    point[1] = val[i];
  }

  /**
   * Adapt to a new bounding box (not implemented).
   */

  public void setDesiredBoundingBox(BoundingBox b)
  {
  }

  /**
   * Returns the x-value of sample i. The samples are numbered starting
   * from zero to getSize()-1.
   */

  public double getX(int i)
  {
    return arg[i];
  }

  /**
   * Returns the y-value of sample i. The samples are numbered starting
   * from zero to getSize()-1.
   */

  public double getY(int i)
  {
    return val[i];
  }

  /**
   * Get all x values as an array. This is more efficient than
   * calling getX multiple times.
   */

  public double[] getXValues()
  {
    return arg;
  }

  /**
   * Get all y values as an array. This is more efficient than
   * calling getY multiple times.
   */

  public double[] getYValues()
  {
    return val;
  }

  /**
   * Set the x-value of sample i.
   */

  public void setX(int i, double x)
  {
    arg[i]=x;
  }

  /**
   * Set the y-value of sample i.
   */

  public void setY(int i, double y)
  {
    val[i]=y;
  }

  // Private members

  private int size;
  private double[] arg;
  private double[] val;
  private Function function; 
}
