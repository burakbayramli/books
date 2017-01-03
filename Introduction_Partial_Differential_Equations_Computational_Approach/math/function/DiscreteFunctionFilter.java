package ifi.math.function;

/**
 * The FunctionFilter is an interface for objects that are capable of
 * performing filtering operations on objects that implement the
 * <a href="mathtools.PlottableFunction.html" PlottableFunction</a>
 * interface.
 */

public interface DiscreteFunctionFilter
{
  /**
   * Perform filtering of the input function, and
   * return a new objects that represents the filtered function.
   */

  void filter(DiscreteFunction in, DiscreteFunction out);
}
