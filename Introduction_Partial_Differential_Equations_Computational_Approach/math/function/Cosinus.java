package ifi.math.function;

/**
  * <em>Cosinus</em> is a simple implementation of a cosinus function.
  * The wave length and the amplitude may be set. Default values are 1.0.
  * @author Åsmund Ødegård
  */

public class Cosinus extends Function
{
	/**
	 * Construct a sinus.
	 */
	public Cosinus()
	{
	}
	
	public Cosinus( double w, double a ) 
	{
		wave = w;
		ampl = a;
	}
	
	/**
	 * The main function of this class.
	 */
	public double value ( double x ) 
	{
		return ampl * Math.cos ( wave * x );
	}

	public void setWave ( double w )
	{
		wave = w;
	}
	
	public double getWave ()
	{
		return wave;
	}
	
	public void setAmpl ( double a )
	{
		ampl = a;
	}
	
	public double getAmpl ()
	{
		return ampl;
	}

	// Private members
	private double wave = 1.0;
	private double ampl = 1.0;
}
