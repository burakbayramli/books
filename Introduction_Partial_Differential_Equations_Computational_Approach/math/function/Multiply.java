package ifi.math.function;

public class Multiply extends Functional
{
	public Multiply( Function l, Function r )
	{
		super (l, r);
	}
	
	public double value ( double x )
	{
		return leftOperand.value(x) * rightOperand.value(x);
	}
}
