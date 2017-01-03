package ifi.math.function;

/**
 * <em>TimeDepFunction</em> implements a analytical function which has a
 * parameter. Of course, the parameter can be what ever you want, but we
 * choose the term "time" by obvious reasons. The user must supplie a Function
 * at construction, which provides the values. 
 * @author Åsmund Ødegård
 */

public class TimeDepFunction extends Function
{
	
	/**
	 * Construct a TimeFunction with values from a given Function. The start
	 * time is set to zero.
	 */
	public TimeDepFunction ( Function f)
	{
		myFunction = f;
		time = 0.0;
	}
	
	/**
	 * Construct a TimeFunction with given Function and start time.
	 */
	public TimeDepFunction ( Function f, double t)
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
		return myFunction.value(x);
	}

	/**
	 * Set the time parameter.
	 */
	public void setTime ( double t ) 
	{
		time = t;
		/* If myFunction is a filter, it may contain either TimeFunction or
		 * TimeDepFunction. Hence we must check for this, and set the time.
		 * The variable myFunction may be a TimeFunction as well. This is pretty
		 * senseless, but we include the case. 
		 */
		if ( myFunction.isA().equals("Functional") )
		{
			((Functional)myFunction).setTime(time);
		}
		else if ( myFunction.isA().equals("TimeFunction") )
		{
			((TimeFunction)myFunction).setTime(time);
		}
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

	public String isA () 
	{
		return "TimeDepFunction";
	}

	// Private members.

	private double time;
	private double step; // For iterations, animations etc.
	private Function myFunction; 
}
