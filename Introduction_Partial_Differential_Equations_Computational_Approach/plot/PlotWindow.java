package ifi.plot;

import ifi.plot.PlotArea;
import ifi.plot.BoundingBox;
import ifi.plot.TickMark;

import java.awt.*;
import java.util.Vector;

/**
 * PlotWindows can be used to create simple scientific plots with
 * a title and labels for the axes. A PlotArea is used for the actual
 * plot. This class contains severeal methods which are identical to the
 * ones provided by PlotArea. These methods calls their PlotArea
 * conterpart directly, and are provided for the convinience of the users.
 */

public class PlotWindow extends Panel
{
  /**
   * Creates a new PlotWindow. The default window contains a canvas only.
   * Title and labels can be added using setTitle, setXLabel and setYLabel.
   *
   * @see PlotWindow#setTitle PlotWindow#setXLabel PlotWindow#setYLabel
   */

  public PlotWindow()
  {
    init(1);
  }

  /**
   * Creates a new PlotWindow. The default window contains a canvas only.
   * Title and labels can be added using setTitle, setXLabel and setYLabel.
   *
   * @see PlotWindow#setTitle PlotWindow#setXLabel PlotWindow#setYLabel
   */

  public PlotWindow(int no_areas)
  {
    init(no_areas);
  }

  /**
   * Init plot window.
   */

  void init(int no_areas)
  {
    setLayout(new GridBagLayout());
    
    plotarea = new PlotArea[no_areas];
    ylabel = new Label[no_areas];

    number_of_plot_areas = no_areas;
    current_area = 0;

    int i;
    for(i=0 ; i < no_areas ; i++)
    {
      plotarea[i] = new PlotArea();

      gbc.gridx = 1;
      gbc.gridy = i+1;
      gbc.weightx = 1.0;

      if(i==0) gbc.weighty = 3.0/no_areas;
      else gbc.weighty = 1.0/no_areas;

      gbc.fill = GridBagConstraints.BOTH;

      add(plotarea[i], gbc);
    }

    gbc.weightx = 0.0;  // reset to default
    gbc.weighty = 0.0;

    doLayout();
    validate();
  }

  /**
   * Set a title for the current plot. The title is shown centered
   * above the plot area.
   */

  public void setTitle(String s)
  {
    if( title==null )
    {
      title = new Label(s);
      title.setAlignment(Label.CENTER);

      gbc.gridx = 1;
      gbc.gridy = 0;
      gbc.fill  = GridBagConstraints.HORIZONTAL;

      add(title, gbc);
    }
    else
      title.setText(s);

    validate();
  }

  /**
   * Set the label for the x-axis. The label is shown centered
   * below the plot area.
   */

  public void setXLabel(String s)
  {
    if( xlabel==null )
    {
      xlabel = new Label(s);
      xlabel.setAlignment(Label.CENTER);

      gbc.gridx = 1;
      gbc.gridy = number_of_plot_areas+1;
      gbc.fill  = GridBagConstraints.HORIZONTAL;

      add(xlabel, gbc);
    }
    else
      xlabel.setText(s);

    validate();
  }

  /**
   * Set the label for the y-axis. The label is shown centered
   * to the left of the plot area.
   */

  public void setYLabel(String s)
  {
    if( ylabel[current_area]==null )
    {
      ylabel[current_area] = new Label(s);
      ylabel[current_area].setAlignment(Label.CENTER);

      gbc.gridx = 0;
      gbc.gridy = current_area+1;
      gbc.fill  = GridBagConstraints.VERTICAL;

      add(ylabel[current_area], gbc);
    }
    else
      ylabel[current_area].setText(s);

    validate();
  }

  /**
   * Set manual or automatic generation of tickmarks for the
   * x-axis. The automaticly generated tick marks will be numerical,
   * while the manual tickmarks may contains arbitrary texts.
   * The label part of each tickmark will be centered below the tick.
   */

  public void setAutoXTicks(boolean b)
  {
    plotarea[current_area].setAutoXTicks(b);
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
    plotarea[current_area].setAutoYTicks(b);
  }

  /**
   * Add a tickmark that is associated with the x-axis.
   */

  public void addXTick(TickMark t)
  {
    plotarea[current_area].addXTick(t);
  }

  /**
   * Add a tickmark that is associated with the y-axis.
   */

  public void addYTick(TickMark t)
  {
    plotarea[current_area].addYTick(t);
  }

  /**
   * Set current plot area
   */

  public void setCurrentPlotArea(int i)
  {
    if(i>=0 && i<number_of_plot_areas) current_area = i;
  }

  /**
   * Add a function to the collection of functions to be plotted.
   * The function will be plotted using the curent default style.
   */

  public void addFunction(PlotableFunction f)
  {
    plotarea[current_area].addFunction(f);
    validate();
  }

  /**
   * Add a function to the collection of functions to be plotted.
   * The function will be plotted using the specified style.
   */

  public void addFunction(PlotableFunction f, PlotStyle s)
  {
    plotarea[current_area].addFunction(f,s);
  }

  /**
   * Remove a function from the collection of functions to be plotted.
   * If this is the last function in the set, all information abound
   * bounds and tickmarks will be deleted.
   */

  public void removeFunction(PlotableFunction f)
  {
    plotarea[current_area].removeFunction(f);
  }

  /**
   * Emty the collection of functions to be plotted.
   * All information abound bounds and tickmarks are also deleted
   */

  public void removeAllFunctions()
  {
    int i;
    for(i=0 ; i < number_of_plot_areas ; i++)
      if(plotarea[i] != null) plotarea[i].removeAllFunctions();
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
    plotarea[current_area].calculateBounds();
  }

  public void setBoundingBox(BoundingBox b)
  {
    plotarea[current_area].setBoundingBox(b);
  }

  /**
   * Update the contents of the plotwindow.
   */

  public void updatePlot()
  {
    int i;
    for(i=0 ; i < number_of_plot_areas ; i++)
      if(plotarea[i] != null) plotarea[i].updatePlot();
  }

  /**
   * Update the contents of the plotwindow.
   */

  public void paint(Graphics g)
  {
    // System.out.println("PlotWindow::paint");
    int i;
    for(i=0 ; i < number_of_plot_areas ; i++)
      if(plotarea[i] != null) plotarea[i].repaint();
  }

  // Data members (private)

  Label   title;
  Label   xlabel;
  Label[] ylabel;

  PlotArea[] plotarea;
  int number_of_plot_areas;
  int current_area;

  static GridBagConstraints gbc = new GridBagConstraints();
}
