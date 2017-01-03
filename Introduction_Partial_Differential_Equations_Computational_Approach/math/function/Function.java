package ifi.math.function;

/**
 * <em>Function</em> implements an analytic function in one dimension. This is
 * an abstract class, which store the left and right boundary of the function,
 * and provides some function. The method which implements the actual
 * expression of the function, is abstract, and must be redefined in a
 * subclass. 
 * @author Åsmund Ødegård
 */

public abstract class Function
{
	/**
	 * Construct a Function. An empty constructor.
	 */
	public Function ()
	{
	}
	
	/**
	 * A constructor which takes the left and right boundaries as arguments.
	 */
	public Function ( double l, double r)
	{
		left = l;
		right = r;
	}

	/**
	 * The main function. This function return the value in the point x. Must
	 * be declared in a subclass.
	 * @param The x value, as a double.
	 * @return The function value, as a double.
	 */
	public abstract double value (double x);
	
	/**
	 * Set the left boundary. The default value is the minimal double value.
	 */
	public void setLeft ( double x)
	{
		left = x;
	}
	
	public double getLeft ()
	{
		return left;
	}
	
	/**
	 * Set the right boundary. The default is the maximal double value.
	 */
	public void setRight ( double x )
	{
		right = x;
	}
	
	public double getRight ()
	{
		return right;
	}

	public String isA()
	{
		return "Function";
	}
	
	// Private members,

	/**
	 * Initialy is both left and right boundaries set to the extreme values.
	 */
	private double left  = -Double.MAX_VALUE;
	private double right = Double.MAX_VALUE;
}
