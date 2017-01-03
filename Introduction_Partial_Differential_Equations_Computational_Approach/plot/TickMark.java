package ifi.plot;

import java.util.Vector;

/**
 * <em>TickMark</em> objects contain information about a single tickmark.
 * Each tickmark consists of a position and a label.
 * The PlotArea class is able to draw tickmarks onto the axis of a plot.
 *
 * The <em>TickMark</em> class also contains a method to generate
 * numerical tickmarks automaticly.
 */

public class TickMark
{
  /**
   * Create a tickmark with specified position and label.
   *
   * @param p position of the label. The position is in "world" coordinates.
   * The PlotArea class will convert this position to "screen" coordinates.
   *
   * @param l the label associated with position p.
   */

  public TickMark(double p, String l)
  {
    pos   = p;
    label = l;
  }

  /**
   * Returns the position of the tickmark.
   */

  public double getPos()
  {
    return pos;
  }

  /**
   * Returns the label associated with the tickmark.
   */

  public String getLabel()
  {
    return label;
  }

  /**
   * Create numerical tickmarks in a specified interval. The
   * distance between two tickmarks will be constant. The position
   * of the tickmarks are created so that they are as "round" as possible.
   * (i.e. we want 100, 110, 120, .... instead of 98, 111, 124, ...)
   *
   * @param start the start value of the interval.
   *
   * @param stop the end value of the interval.
   *
   * @param num desired number of tickmarks. Actual number of tickmarks
   * will vary since we prefere "round" numbers.
   *
   * @param ticks the generated tickmarks will be added to this vector.
   * The vectors is not emptied before adding the tickmarks.
   */

  public static void createTickMarks(double start, double stop, int num,
				     Vector ticks)
  {
    double delta = (stop-start)/num;

    double a = Math.floor(Math.log(delta)/Math.log(10));
    double step = Math.floor(delta/Math.pow(10,a))*Math.pow(10,a);

    double coord = Math.floor(start/step)*step;

    if(coord<start) coord += step;
    while(coord <= stop)
    {
      String label;

      if( a>=0 )
	label = String.valueOf((int) coord);
      else
	label = String.valueOf(coord);

      ticks.addElement(new TickMark(coord, label));

      coord += step;
    }
  }

  double pos;
  String label;
}
