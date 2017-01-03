package ifi.math.function;

/**
  * <em>Sinus</em> is a simple implementation of a sunis function. The
  * wave length and the amplitude may be set. Default values are 1.0.
  * @author Åsmund Ødegård
  */

public class Sinus extends Function
{
	/**
	 * Construct a sinus.
	 */
	public Sinus()
	{
	}
	
	public Sinus( double w, double a ) 
	{
		wave = w;
		ampl = a;
	}
	
	/**
	 * The main function of this class.
	 */
	public double value ( double x ) 
	{
		return ampl * Math.sin ( wave * x );
	}

	public void setWave ( double w)
	{
		wave = w;
	}
	public double getWave()
	{
		return wave;
	}
   public void setAmpl ( double a)
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
