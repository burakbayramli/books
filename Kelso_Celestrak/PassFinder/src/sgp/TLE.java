package sgp;

/**
 *	Clase encargada de almacenar y hacer accesibles los campos de las
 *  dos lineas con formato TLE. Dichos campos contienen los datos necesarios
 *  para conocer la posición de los satélites.
 *
 *  @author Pedro J. Fernández
 *  @author Dr TS Kelso
 *  @version 1.0
 *
 */

public class TLE {
	/** Nombre del satélite */
	private String nombreSatelite;

	/** Primera línea TLE */
	private String linea1TLE;

	/** Segunda línea TLE */
	private String linea2TLE;


	/**
	 * Constructor.
	 *
	 * @param nombre El nombre del satélite contenido en la linea anterios a las linesa TLE.
	 * @param linea1 Primera linea TLE intacta.
	 * @param linea2 Segundo línea TLE intacta.
	 */
	public TLE (String nombre, String linea1, String linea2)
	{
		nombreSatelite = nombre;
		linea1TLE = linea1;
		linea2TLE = linea2;
	}


	/* METODOS DE CONSULTA DE LINEAS COMPLETAS */

	/**
	 * Método que devuelve la línea del nombre del satélite.
	 *
	 * @return El nombre del satélite.
	 *
	 */
	public String getNombreSatelite()
	{
		return nombreSatelite;
	}


	/**
	 * Método que devuelve la primera línea TLE del satélite.
	 *
	 * @return La primera línea TLE.
	 *
	 */
	public String getLinea1TLE()
	{
		return linea1TLE;
	}


	/**
	 * Método que devuelve la segunda línea TLE del satélite.
	 *
	 * @return La segunda línea TLE.
	 *
	 */
	public String getLinea2TLE()
	{
		return linea2TLE;
	}


	/* METODOS DE CONSULTA DE LOS CAMPOS DE LA PRIMERA LINEA */

	/**
	 * Método que devuelve el número de catálodo del NORAD del satélite.
	 *
	 * @return El campo con el número de catalogo.
	 *
	 */
	String getCatnr()
	{
		return linea1TLE.substring(2,7);
	}


	/**
	 * Método que devuelve la variable "Epoch"
	 *
	 * @return Devuelve "Epoch"
	 * @exception NumberFormatException Salta si "Epoch" no es un real.
	 */
	double getEpoch() throws NumberFormatException
	{
		return Double.parseDouble(linea1TLE.substring(18,32));
	}


	/**
	 * Método que devuelve la variable "Xndt2o"
	 *
	 * @return Devuelve "Xndt2o"
	 * @exception NumberFormatException Salta si "Xndt2o" no es un real.
	 */
	double getXndt2o() throws NumberFormatException
	{
		return Double.parseDouble(linea1TLE.substring(33,43));
	}


	/**
	 * Método que devuelve la variable "Xndd6o"
	 *
	 * @return Devuelve "Xndd6o"
	 * @exception NumberFormatException Salta si "Xndd6o" no es un real.
	 */
	double getXndd6o() throws NumberFormatException
	{
		return (Double.parseDouble(linea1TLE.substring(44,50))*1e-5d);
	}


	/**
	 * Método que devuelve la variable "Iexp"
	 *
	 * @return Devuelve "Iexp"
	 * @exception NumberFormatException Salta si "Iexp" no es un entero.
	 */
	int getIexp() throws NumberFormatException
	{
		if (linea1TLE.substring(50,51).equals("+"))
			return Integer.parseInt(linea1TLE.substring(51,52));
		else
			return Integer.parseInt(linea1TLE.substring(50,52));
	}


	/**
	 * Método que devuelve la variable "Bstar"
	 *
	 * @return Devuelve "Bstar"
	 * @exception NumberFormatException Salta si "Bstar" no es un real.
	 */
	double getBstar() throws NumberFormatException
	{
		return (Double.parseDouble(linea1TLE.substring(53,59))*1e-5d);
	}


	/**
	 * Método que devuelve la variable "Ibexp"
	 *
	 * @return Devuelve "Ibexp"
	 * @exception NumberFormatException Salta si "Ibexp" no es un entero.
	 */
	int getIbexp() throws NumberFormatException
	{
		if (linea1TLE.substring(59,60).equals("+"))
			return Integer.parseInt(linea1TLE.substring(60,61));
		else
			return Integer.parseInt(linea1TLE.substring(59,61));
	}


	/**
	 * Método que devuelve la variable "Elset"
	 *
	 * @return Devuelve "Elset"
	 */
	String getElset()
	{
			return linea1TLE.substring(65,69);
	}


	/* METODOS DE CONSULTA DE LOS CAMPOS DE LA SEGUNDA LINEA */

	/**
	 * Método que devuelve la variable "Xincl"
	 *
	 * @return Devuelve "Xincl"
	 * @exception NumberFormatException Salta si "Xincl" no es un real.
	 */
	double getXincl() throws NumberFormatException
	{
		return Double.parseDouble(linea2TLE.substring(8,16));
	}


	/**
	 * Método que devuelve la variable "Xnodeo"
	 *
	 * @return Devuelve "Xnodeo"
	 * @exception NumberFormatException Salta si "Xnodeo" no es un real.
	 */
	double getXnodeo() throws NumberFormatException
	{
		return Double.parseDouble(linea2TLE.substring(17,25));
	}


	/**
	 * Método que devuelve la variable "Eo"
	 *
	 * @return Devuelve "Eo"
	 * @exception NumberFormatException Salta si "Eo" no es un real.
	 */
	double getEo() throws NumberFormatException
	{
		return (Double.parseDouble(linea2TLE.substring(26,33))*1e-7d);
	}


	/**
	 * Método que devuelve la variable "Omegao"
	 *
	 * @return Devuelve "Omegao"
	 * @exception NumberFormatException Salta si "Omegao" no es un real.
	 */
	double getOmegao() throws NumberFormatException
	{
		return Double.parseDouble(linea2TLE.substring(34,40));
	}


	/**
	 * Método que devuelve la variable "Xmo"
	 *
	 * @return Devuelve "Xmo"
	 * @exception NumberFormatException Salta si "Xmo" no es un real.
	 */
	double getXmo() throws NumberFormatException
	{
		return Double.parseDouble(linea2TLE.substring(43,51));
	}


	/**
	 * Método que devuelve la variable "Xno"
	 *
	 * @return Devuelve "Xno"
	 * @exception NumberFormatException Salta si "Xno" no es un real.
	 */
	double getXno() throws NumberFormatException
	{
		return Double.parseDouble(linea2TLE.substring(52,63));
	}

}


