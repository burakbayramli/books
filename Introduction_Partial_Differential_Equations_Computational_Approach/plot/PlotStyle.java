package ifi.plot;

import java.awt.Color;

/**
 * Stores information on how to plot functions. The color and the
 * line type can be set.
 */

public class PlotStyle
{
  /**
   * Create a plotstyle object with default properties.
   * (black functions consisting of lines).
   */

  public PlotStyle()
  {
  }

  /**
   * The color used to plot the function.
   */

  public Color color = Color.black;

  /**
   * How to interpret (and draw) the samples of the function.
   */

  public int draw_type = LINES;

  /**
   * Draw samples using dots.
   */

  public static final int DOTS     = 1;

  /**
   * Draw samples using small circles.
   */

  public static final int POINTS   = 2;

  /**
   * Draw lines between sample i and point i+1. A total of
   * numSamples-1 continious line segments will be drawn.
   */

  public static final int LINES    = 3;

  /**
   * Draw lines between sample 2*i and 2*i+1. A total of
   * numSamples/2 line segments will be drawn. Each segment
   * may be dicontinous from the other segments.
   */

  public static final int SEGMENTS = 4;
}
