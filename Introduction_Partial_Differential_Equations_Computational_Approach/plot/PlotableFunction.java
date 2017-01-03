package ifi.plot;

/**
 * Interface for objects that can be plotted by PlotWindow.
 */

public interface PlotableFunction
{
  /**
   * Return the number of points in which the function can be sampled.
   * This number is relative to the current desired bounding box.
   */

  public int numSamplingPoints();

  /**
   * Returns the x and y values of the i-th sample if the function.
   * The index is relative to the current desired bounding box.
   */

  public void getSample(int i, double[] point);

  /**
   * Adapt the samples to the given bounding box. It is not required
   * to implement this function (i.e. it may be empty).
   * By implementing this function rendering performance may
   * be improved.
   */

  public void setDesiredBoundingBox(BoundingBox b);
}
