package ifi.math.function;

public class Add extends Functional
{
	public Add ( Function l, Function r )
	{
		super (l, r);
	}
	
	public double value ( double x )
	{
		return leftOperand.value(x) + rightOperand.value(x);
	}
}
