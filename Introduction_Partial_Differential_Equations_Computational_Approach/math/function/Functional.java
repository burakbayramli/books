package ifi.math.function;

/**
 * <em>Functional</em> apply some operation to two input
 * functions. The Functional extends Function, and hence it is abstract. 
 * @author Åsmund Ødegård
 */

public abstract class Functional extends Function
{
	/**
	 * Construct a filter with given left and right functions.
	 */
	public Functional ( Function l, Function r) 
	{
		leftOperand = l;
		rightOperand = r;
	}

	public void setTime ( double t )
	{
		if ( leftOperand.isA().equals("Filter") )
			((Functional)leftOperand).setTime(t);
		else if ( leftOperand.isA().equals("TimeFunction") )
			((TimeFunction)leftOperand).setTime(t);
		else if ( leftOperand.isA().equals("TimeDepFunction") )
			((TimeDepFunction)leftOperand).setTime(t);
		if ( rightOperand.isA().equals("Functional") )
			((Functional)rightOperand).setTime(t);
		else if ( rightOperand.isA().equals("TimeFunction") )
			((TimeFunction)rightOperand).setTime(t);
		else if ( rightOperand.isA().equals("TimeDepFunction") )
			((TimeDepFunction)rightOperand).setTime(t);
	}

	public String isA ()
	{
		return "Functional";
	}
	

	// Private members.
	
	Function leftOperand;
	Function rightOperand;
}
