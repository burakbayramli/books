package ifi.math.function;

public class Minus extends Functional
{
	public Minus( Function l, Function r )
	{
		super (l, r);
	}
	
	public double value ( double x )
	{
		return leftOperand.value(x) - rightOperand.value(x);
	}
}
