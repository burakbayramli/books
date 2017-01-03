package ifi.math.function;

/**
 * This is simply an implementation of a linear function. 
 */

public class Linear extends Function
{

	/**
	 * The default constructor. A = 1.0 and B = 0.0
	 */
	public Linear()
	{
	}
	
	/**
	 * Construct the linear function y = ax + b.
	 */
	public Linear( double a, double b )
	{
		A = a;
		B = b;
	}
	
	public double value ( double x )
	{
		return A*x + B;
	}
	
	public void setA ( double a ) 
	{
		A = a;
	}
	
	public double getA ()
	{
		return A; 
	}
	
	public void setB ( double b ) 
	{
		B = b;
	}
	
	public double getB ()
	{
		return B;
	}

	
	// Private members
	private double A = 1.0;
	private double B = 0.0;
}
