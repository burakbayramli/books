package ifi.math.function;

/**
 * <em>TimeFunction</em> implements a analytical function which has a
 * parameter. Of course, the parameter can be what ever you want, but we
 * choose the term "time" by obvious reasons. The user must supplie a Function
 * at construction, which provides the values.
 * @author Åsmund Ødegård
 */

public class TimeFunction extends Function
{
	
	/**
	 * Construct a TimeFunction with values from a given Function. The start
	 * time is set to zero.
	 */
	public TimeFunction ( Function f)
	{
		myFunction = f;
		time = 0.0;
	}
	
	/**
	 * Construct a TimeFunction with given Function and start time.
	 */
	public TimeFunction ( Function f, double t)
	{
		myFunction = f;
		time = t;
	}
	
	/**
	 * The value of this Function. This method is required since we extend
	 * Function.
	 */
	public double value (double x )
	{
		// The parameter should not be used.
		return myFunction.value(time);
	}

	/**
	 * Set the time parameter.
	 */
	public void setTime ( double t ) 
	{
		time = t;
	}
	
	/**
	 * Get current time variable.
	 */
	public double getTime () 
	{
		return time;
	}
	
	/**
	 * Increment the time.
	 */
	public void incrementTime ()
	{
		time += step;
	}

	/**
	 * Set the time step.
	 */
	public void setStep ( double s ) 
	{
		step = s;
	}
	
	/**
	 * Get current step value.
	 */
	public double getStep ()
	{
		return step;
	}

	public String isA()
	{
		return "TimeFunction";
	}

	// Private members.

	private double time;
	private double step; // For iterations, animations etc.
	private Function myFunction; 
}
