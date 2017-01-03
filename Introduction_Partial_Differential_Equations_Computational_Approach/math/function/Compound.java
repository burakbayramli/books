package ifi.math.function;

public class Compound extends Functional
{
	public Compound ( Function l, Function r )
	{
		super (l, r);
	}
	
	public double value ( double x )
	{
		return leftOperand.value(rightOperand.value(x));
	}
}
