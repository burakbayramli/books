package ifi.plot;

import java.awt.*;
import java.util.Vector;

/**
 * PlotArea's can be used to create simple scientific plots.
 * An arbitrary number of functions may be plotted.
 * The axis can be fixed or autoscaled. Numerical tickmarks can be
 * generated automaticly. Textual tickmarks can be provided by
 * the user.
 *
 * This initial plot area will have zero size. The
 * size should preferably be controlled by a layout manager that provides
 * dynamical resizeing of its components. It is also possible to
 * use the setSize, setPreferedSize and setMinimumSize methods in
 * Component.
 *
 */

class PlotArea extends Canvas
{
  /**
   * Create a plotarea. The plotarea will automaticly generate tickmarks,
   * and will autoscale itself so that all functions fits inside the area.
   */

  public PlotArea()
  {
  }

  /* Create a plotarea. The plotarea will generate tickmarks acording to the
   * boolean values provided. The plotarea will autoscale itself so
   * that all functions fits inside the area.
   */

  public PlotArea(boolean auto_x_ticks, boolean auto_y_ticks)
  {
    this.auto_x_ticks = auto_x_ticks;
    this.auto_y_ticks = auto_y_ticks;
  }

  /**
   * Set manual or automatic generation of tickmarks for the
   * x-axis. The automaticly generated tick marks will be numerical,
   * while the manual tickmarks may contains arbitrary texts.
   * The label part of each tickmark will be centered below the tick.
   */

  public void setAutoXTicks(boolean b)
  {
    auto_x_ticks = b;
  }

  /**
   * Set manual or automatic generation of tickmarks for the
   * y-axis. The automaticly generated tick marks will be numerical,
   * while the manual tickmarks may contains arbitrary texts.
   * The label part of each tickmark will be centered left to the left
   * the tick.
   */

  public void setAutoYTicks(boolean b)
  {
    auto_y_ticks = b;
  }

  /**
   * Add a tickmark that is associated with the x-axis.
   */

  public void addXTick(TickMark t)
  {
    xticks.addElement(t);
  }

  /**
   * Add a tickmark that is associated with the y-axis.
   */

  public void addYTick(TickMark t)
  {
    yticks.addElement(t);
  }

  /**
   * Add a function to the collection of functions to be plotted.
   * The function will be plotted using the curent default style.
   */

  public void addFunction(PlotableFunction f)
  {
    functions.addElement(f);

    PlotStyle def = new PlotStyle();
    styles.addElement(def);
  }

  /**
   * Add a function to the collection of functions to be plotted.
   * The function will be plotted using the specified style.
   */

  public void addFunction(PlotableFunction f, PlotStyle style)
  {
    functions.addElement(f);
    styles.addElement(style);
  }

  /**
   * Remove a function from the collection of functions to be plotted.
   * If this is the last function in the set, all information abound
   * bounds and tickmarks will be deleted.
   */

  public void removeFunction(PlotableFunction f)
  {
    int indx = functions.indexOf(f);

    functions.removeElementAt(indx);
    styles.removeElementAt(indx);

    if(functions.size()==0)
    {
      bounds = null;
      xticks.removeAllElements();
      yticks.removeAllElements();
    }
  }

  /**
   * Emty the collection of functions to be plotted.
   * All information abound bounds and tickmarks are also deleted
   */

  public void removeAllFunctions()
  {
    functions.removeAllElements();
    styles.removeAllElements();

    if(functions.size()==0)
    {
      bounds = null;
      xticks.removeAllElements();
      yticks.removeAllElements();
    }
  }

  /**
   * Calculate and store the extent of all functions curently
   * in the collection of functions to be plotted. This bounding
   * box defined by this extent will be used when plotting the
   * functions. Functions added after caling this method will
   * not influence the bounding box used when plotting.
   */

  public void calculateBounds()
  {
    bounds = new BoundingBox();
    updateBounds(bounds);

    updateXTicks(bounds);
    updateYTicks(bounds);
  }

  protected void updateXTicks(BoundingBox b)
  {
    if(auto_x_ticks)
    {
      int no_x_ticks = getBounds().width / 100;
      if(no_x_ticks<2) no_x_ticks=2;

      xticks.removeAllElements();
      TickMark.createTickMarks(b.getMinX(), b.getMaxX(), 
			       no_x_ticks, xticks);
    }
  }

  protected void updateYTicks(BoundingBox b)
  {
    if(auto_y_ticks)
    {
      int no_y_ticks = getBounds().height / 75;
      if(no_y_ticks<2) no_y_ticks=2;

      yticks.removeAllElements();
      TickMark.createTickMarks(b.getMinY(), b.getMaxY(), 
			       no_y_ticks, yticks);
    }
  }

  /**
   * Set the bounding box
   */

  public void setBoundingBox(BoundingBox b)
  {
    bounds = b;
    updateXTicks(b);
    updateYTicks(b);
  }

  /**
   * Update the provided bounding box so that all functions
   * in the collections of plottable functions fits whitin
   * the bounding box.
   */

  protected void updateBounds(BoundingBox bb)
  {
    int i;
    for(i=0 ; i < functions.size(); i++)
      bb.update((PlotableFunction) functions.elementAt(i));
  }

  /**
   * Redraw all functions and axes.
   */

  public void paint(Graphics g)
  {
    //    System.out.println("PlotArea::paint");

    // Doube buffer init

    Dimension d = getSize();

    if(hidden==null || hidden.getWidth(this)!=d.width || 
       hidden.getHeight(this)!=d.height)
    {
      updateHiddenImage();
    }

    g.drawImage(hidden, 0, 0, this);
  }

  /**
   *
   */

  public void updatePlot()
  {
    updateHiddenImage();
    repaint();
  }

  /**
   *
   */

  private void updateHiddenImage()
  {
    int i,j;

    // Find size and position of canvas

    Dimension d = getSize();

    if(hidden==null || hidden.getWidth(this)!=d.width || 
       hidden.getHeight(this)!=d.height)
    {
      if(d.width==0 && d.height==0) 
	hidden = createImage(100, 100);
      else if(d.width==0)
	hidden = createImage(100, d.height);
      else if(d.height==0)
	hidden = createImage(d.width, 100);
      else
	hidden = createImage(d.width, d.height);

      hiddenG = hidden.getGraphics();
    }

    int max_char_height = hiddenG.getFontMetrics().getMaxAscent() +
                          hiddenG.getFontMetrics().getMaxDescent();

    int max_char_width = hiddenG.getFontMetrics().getMaxAdvance();

    int canvas_width  = d.width  - 10*max_char_width;
    int canvas_height = d.height - 3*max_char_height;

    int canvas_left   = 8*max_char_width;
    int canvas_top    = max_char_height;
    int canvas_bottom = canvas_top + canvas_height;

    // Clear old image

    hiddenG.clearRect(0, 0, d.width, d.height);
    hiddenG.setColor(Color.black);

    // Draw coordinate system

    if( functions.size() > 0 )
    {
      hiddenG.drawLine(canvas_left, canvas_bottom,
		       canvas_left, canvas_top);

      hiddenG.drawLine(canvas_left, canvas_bottom,
		       canvas_left+canvas_width, canvas_bottom);
    }

    // Calculate scaling and translations

    BoundingBox bb;

    if( bounds==null )
    {
      bb = new BoundingBox();
      updateBounds(bb);
    }
    else
    {
      if( bounds.getWidth() < 0 || bounds.getHeight() < 0 )
      {
	bb = new BoundingBox();
	updateBounds(bb);

	if( bounds.getWidth() > 0 )
	{
	  bb.setMinX(bounds.getMinX());
	  bb.setMaxX(bounds.getMaxX());
	}

	if( bounds.getHeight() > 0 )
	{
	  bb.setMinY(bounds.getMinY());
	  bb.setMaxY(bounds.getMaxY());
	}

	updateXTicks(bb);
	updateYTicks(bb);
      }
      else
	bb = bounds;
    }

    double sx =   canvas_width / bb.getWidth();
    double sy = - canvas_height / bb.getHeight();

    double tx = canvas_left - bb.getMinX()/bb.getWidth() *
                canvas_width;
    double ty = canvas_bottom + bb.getMinY()/bb.getHeight() *
                canvas_height;

    // Draw tickmarks

    Vector xt = new Vector();
    if(auto_x_ticks && bounds==null)
      TickMark.createTickMarks(bb.getMinX(), bb.getMaxX(), 5, xt);
    else
      xt = xticks;

    for(i=0 ; i < xt.size() ; i++)
    {
      double x = ((TickMark) xt.elementAt(i)).getPos();

      if(x>=bb.getMinX() &&x<=bb.getMaxX())
      {
	int screen_x = (int) (sx*x + tx);

	hiddenG.drawLine(screen_x, canvas_bottom-3, screen_x, canvas_bottom+3);

	String label = ((TickMark) xt.elementAt(i)).getLabel();
	int label_width = hiddenG.getFontMetrics().stringWidth(label);

	hiddenG.drawString(label, screen_x-label_width/2, canvas_bottom+3+
			   max_char_height);
      }
    }

    Vector yt = new Vector();
    if(auto_y_ticks && bounds==null)
      TickMark.createTickMarks(bb.getMinY(), bb.getMaxY(), 5, yt);
    else
      yt = yticks;

    for(i=0 ; i < yt.size() ; i++)
    {
      double y = ((TickMark) yt.elementAt(i)).getPos();

      if(y>=bb.getMinY() && y<=bb.getMaxY())
      {
	int screen_y = (int) (sy*y + ty);

	hiddenG.drawLine(canvas_left-3, screen_y, canvas_left+3, screen_y);

	String label = ((TickMark) yt.elementAt(i)).getLabel();
	int label_width = hiddenG.getFontMetrics().stringWidth(label);

	hiddenG.drawString(label, canvas_left-5-label_width, screen_y);
      }
    }

    // Loop through functions and draw lines

    for(i=0 ; i < functions.size() ; i++)
    {
      PlotableFunction f = (PlotableFunction) functions.elementAt(i);
      PlotStyle style = (PlotStyle) styles.elementAt(i);

      hiddenG.setColor(style.color);

      int n = f.numSamplingPoints();

      f.getSample(0, p);

      int start_x = (int) (sx*p[0]+tx);
      int start_y = (int) (sy*p[1]+ty);

      boolean last_point_ok = false;

      if(p[0]>=bb.getMinX() && p[0]<=bb.getMaxX() && 
	 p[1]>=bb.getMinY() && p[1]<=bb.getMaxY())
      {
	if(style.draw_type == PlotStyle.DOTS )
	  hiddenG.drawLine(start_x, start_y, start_x, start_y);
	else if(style.draw_type == PlotStyle.POINTS )
	  hiddenG.drawOval(start_x-2, start_y-2, 4, 4);

	last_point_ok = true;
      }

      for(j=1 ; j < n ; j++)
      {
	f.getSample(j, p);

	int stop_x = (int) (sx*p[0] + tx);
	int stop_y = (int) (sy*p[1] + ty);

	if(p[0]>=bb.getMinX() && p[0]<=bb.getMaxX() && 
	   p[1]>=bb.getMinY() && p[1]<=bb.getMaxY())
	{
	  if(style.draw_type == PlotStyle.DOTS && last_point_ok)
	    hiddenG.drawLine(stop_x, stop_y, stop_x, stop_y);
	  else if(style.draw_type == PlotStyle.POINTS )
	    hiddenG.drawOval(stop_x-2, stop_y-2, 4, 4);
	  else if(style.draw_type == PlotStyle.LINES  && last_point_ok)
	    hiddenG.drawLine(start_x, start_y, stop_x, stop_y);
	  else if(style.draw_type == PlotStyle.SEGMENTS && j%2==1 && last_point_ok)
	    hiddenG.drawLine(start_x, start_y, stop_x, stop_y);

	  last_point_ok = true;
	}

	// Update for next iteration
	start_x = stop_x;
	start_y = stop_y;
      }
    }
  }

  boolean auto_x_ticks=true;
  boolean auto_y_ticks=true;

  Vector functions = new Vector();
  Vector styles = new Vector();

  Vector xticks = new Vector();
  Vector yticks = new Vector();

  BoundingBox bounds;

  Image    hidden;
  Graphics hiddenG;

  static double[] p = new double[2];
}
