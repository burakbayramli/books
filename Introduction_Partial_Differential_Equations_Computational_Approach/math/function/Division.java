package ifi.math.function;

public class Division extends Functional
{
	public Division ( Function l, Function r )
	{
		super (l, r);
	}
	
	public double value ( double x )
	{
		return leftOperand.value(x) / rightOperand.value(x);
	}
}
