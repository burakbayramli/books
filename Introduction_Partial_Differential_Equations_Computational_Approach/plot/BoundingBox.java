package ifi.plot;

/**
 * Bounding boxes can be used to compute the extent of sets of functions
 * or 2D points. The bounding box is initially set so that has negative
 * extent, it can then be iteratively expanded to fit around functions
 * or 2D points.
 */

public class BoundingBox
{
  /**
   * Construct a bounding box with negative extent. This constructor
   * should always be used together with the <em>update</em> functions.
   */

  public BoundingBox()
  {
    reset();
  }

  /**
   * Construct a bounding box with specified limits.
   */

  public BoundingBox(double low_x, double low_y, double high_x, double high_y)
  {
    min_x = low_x;
    max_x = high_x;

    min_y = low_y;
    max_y = high_y;
  }

  /**
   * Reset the bounding box so that it has negativ extent.
   */

  public void reset()
  {
    min_x =  Double.MAX_VALUE;
    max_x = -Double.MAX_VALUE;
    min_y =  Double.MAX_VALUE;
    max_y = -Double.MAX_VALUE;
  }

  /**
   * Update the bounding box so that it contains the input point.
   */

  public void update(double x, double y)
  {
    if( x < min_x ) min_x = x;
    if( x > max_x ) max_x = x;

    if( y < min_y ) min_y = y;
    if( y > max_y ) max_y = y;
  }

  /**
   * Update the bounding box so that it contains all the samples
   * of the input function.
   */

  public void update(PlotableFunction f)
  {
    int i;
    int n = f.numSamplingPoints();

    for(i=0 ; i < n ; i++)
    {
      f.getSample(i, p);
      update(p[0],p[1]);
    }
  }

  // Return the values of the local variables.

  public final double getMinX() { return min_x; }
  public final double getMaxX() { return max_x; }

  public final double getMinY() { return min_y; }
  public final double getMaxY() { return max_y; }

  public final double getWidth()  { return max_x - min_x; }
  public final double getHeight() { return max_y - min_y; }

  public final void setMinX(double x) { min_x = x; }
  public final void setMaxX(double x) { max_x = x; }

  public final void setMinY(double y) { min_y = y; }
  public final void setMaxY(double y) { max_y = y; }

  // Local variables

  double min_x;
  double max_x;
  double min_y;
  double max_y;

  static double[] p = new double[2];
}

