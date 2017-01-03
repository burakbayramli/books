# Make a gray-scale map based of the values of one vector
  # Defaults to using the longitude and latitude coordinates of the California
  # housing data
  # Can control number of levels for the grey scale
  # Can give an arbitrary vector of break-points for grey levels; code stops if
  # the number of  break points doesn't match the number of levels
  # Without a vector of breaks, the code selects them, either as equally spaced
  # along the z axis, or by percentiles of the z axis
  # Prints a legend showing breaks, unless legend.loc=NULL
# Inputs: vector of greys (z),
  # vectors of horizontal + vertical coordinates (x and y),
  # number of grey levels (n.levels), vector of break-points (breaks),
  # criterion for automatically calculated break-points (break.by),
  # legend location/control (legend.loc), number of significant figures to
  # use in legend (digits)
  # additional graphical parameters (...)
    # Currently, any value for "breaks.by" other than "length" gives breaks
    # by quantiles
# Side-effects: creates a plot
# Outputs: the vector of break-points
   # So that the 
graymapper <- function(z, x=calif$LONGITUDE,y=calif$LATITUDE, n.levels=10,
  breaks=NULL,break.by="length",legend.loc="topright",digits=3,...) {
  # Get the grey levels
  my.greys = grey(((n.levels-1):0)/n.levels)
  # If we're given break-points, use them, unless the number doesn't match up
  # with the number of levels
  if (!is.null(breaks)) {
    stopifnot(length(breaks) == (n.levels+1))
  }
  # Otherwise, calculate the break-points,
  else {
    # either equally spaced,
    if(identical(break.by,"length")) {
      breaks = seq(from=min(z),to=max(z),length.out=n.levels+1)
    # or by quantiles
    } else {
      breaks = quantile(z,probs=seq(0,1,length.out=n.levels+1))
    }
  }
  # Which level does each z value correspond to?
  z = cut(z,breaks,include.lowest=TRUE)   # Consider also findInterval
  # Which colors are those?
  colors = my.greys[z]
  # Make a nice plot with filled-in points
  plot(x,y,col=colors,bg=colors,...)
  # Add a legend, optionally
  if (!is.null(legend.loc)) {
    # We need a global upper limit to use cut(), but nothing ever reaches that
    breaks.printable <- signif(breaks[1:n.levels],digits)
    legend(legend.loc,legend=breaks.printable,fill=my.greys)
  }
  invisible(breaks)
}
