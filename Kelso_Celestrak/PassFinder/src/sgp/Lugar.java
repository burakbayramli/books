// Clase que representa una localización en el Mundo
package sgp;

public class Lugar implements Comparable
{
	public double azimutSol, elevacionSol, rangoSol, ratioRangoSol;
	public String nombre;
	public double latitud, longitud, altitud, offsetUTC;

	double twilight = Cte.twilightCivil;
	double[] polares = new double[4];
	double[] pos = new double[4];
	double[] vel = new double[4];
	private double[] range = new double[4];
	private double[] rgvel = new double[4];

	private String ficheroLOC = "";


	public Lugar (String nom, double lat, double lon, double alt, double off)
	{
		nombre = nom;
		latitud = lat;
		longitud = lon;
		altitud = alt;
		offsetUTC = off;
		polares[0] = Math.toRadians(lat);
		polares[1] = Math.toRadians(FuncionesMatematicas.modulus(lon,360));
		polares[2] = alt * 0.001;
	}

	void setFicheroLOC(String fichero)
	{
		ficheroLOC = fichero;
	}

	String getFicheroLOC()
	{
		return ficheroLOC;
	}

	void setVariables (String nom, double lat, double lon, double alt, double off)
	{
		nombre = nom;
		latitud = lat;
		longitud = lon;
		altitud = alt;
		offsetUTC = off;
		polares[0] = Math.toRadians(lat);
		polares[1] = Math.toRadians(FuncionesMatematicas.modulus(lon,360));
		polares[2] = alt * 0.001;
	}

	public int compareTo(Object o)
	{
		Lugar l = (Lugar) o;
		return nombre.compareTo(l.nombre);
	}

	boolean esDeNoche()
	{
		return (elevacionSol < twilight);
	}

	void calcularPosicionSol(MarcaDeTiempo t)
	{
		calcularPosicionSol(Tiempo.timeToJulianTime(t));
	}

	void calcularPosicionSol(double jt)
	{
		int i;
		double sin_lat,cos_lat, sin_theta,cos_theta;
		double el,azim,lat,theta;
		double top_s,top_e,top_z;

		calcularVariables(jt);
		for (int n=0;n<3;n++)
		{
			range[n] = Sol.pos[n] - pos[n];
			rgvel[n] = 0.0 - vel[n];
		}
		FuncionesMatematicas.magnitud(range);

		lat = polares[0];
		theta = polares[3];
		sin_lat = Math.sin(lat);
		cos_lat = Math.cos(lat);
		sin_theta = Math.sin(theta);
		cos_theta = Math.cos(theta);
		top_s = sin_lat*cos_theta*range[0]
		     + sin_lat*sin_theta*range[1]
		     - cos_lat*range[2];
		top_e = -sin_theta*range[0]
		     + cos_theta*range[1];
		top_z = cos_lat*cos_theta*range[0]
		     + cos_lat*sin_theta*range[1]
		     + sin_lat*range[2];
		azim = Math.atan(-top_e/top_s); //Azimuth
		if (top_s > 0)
			azim = azim + Math.PI;
		if (azim < 0)
			azim = azim + (Math.PI*2.0);
		el = Math.asin(top_z/range[3]);
		azimutSol = Math.toDegrees(azim);  //Azimuth (radians to degrees)
		elevacionSol = el; //Elevation (radians)
		rangoSol = range[3];  //Range (kilometers)
		ratioRangoSol = FuncionesMatematicas.dot(range,rgvel)/range[3]; //Range Rate (kilometers/second)
		// Corrections for atmospheric refraction }
		// Reference:  Astronomical Algorithms by Jean Meeus, pp. 101-104 }
		// Note:  Correction is meaningless when apparent elevation is below horizon }
		elevacionSol = elevacionSol + Math.toRadians((1.02/Math.tan(Math.toRadians(Math.toDegrees(el)+10.3/(Math.toDegrees(el)+5.11))))/60.0);
		if (elevacionSol < 0)
			elevacionSol = el;  //Reset to true elevation
		elevacionSol = Math.toDegrees(elevacionSol);
	}

	void calcularVariables(MarcaDeTiempo t)
	{
		calcularVariables(Tiempo.timeToJulianTime(t));
	}

	void calcularVariables(double jt)
	{
		double c,s,achcp;
		double lat, lon, alt, theta;

		lat = polares[0];
		lon = polares[1];
		alt = polares[2];

		// LMST
		theta = FuncionesMatematicas.modulus(Tiempo.thetaG_JD(jt) + lon,(Math.PI*2));
	  	polares[3]=theta;
		c = 1.0/Math.sqrt(1.0 + Cte.f*(Cte.f - 2.0)*Math.pow(Math.sin(lat),2.0));
		s = ((1 - Cte.f)*(1 - Cte.f))*c;
		achcp = (Cte.xkmper*c + alt)*Math.cos(lat);
		pos[0] = achcp*Math.cos(theta);  // kilometros
		pos[1] = achcp*Math.sin(theta);
		pos[2] = (Cte.xkmper*s + alt)*Math.sin(lat);
		vel[0] = -Cte.mfactor*pos[1]; //kilometers/second
		vel[1] =  Cte.mfactor*pos[0];
		vel[2] =  0;

		FuncionesMatematicas.magnitud(pos);
		FuncionesMatematicas.magnitud(vel);
	}

}
