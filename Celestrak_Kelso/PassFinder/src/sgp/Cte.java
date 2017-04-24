// Constantes más utilizadas por las clases del paquete
// Clase estática que concentra todas las constantes
package sgp;

class Cte
{
	final static double ae = 1.0;
	final static double tothrd = 2.0/3.0;
	// Radio ecuatorial de la tierra en kilómetros
	final static double xkmper = 6378.135;
	// Achatamiento de la tierra
	final static double f = 1.0/298.26;
	// Constante gravitacional de la tierra
	final static double ge = 398600.8;
	// Armónico J2
	final static double J2 = 1.0826158e-3;
	// Armónico J3
	final static double J3 = -2.53881e-6;
	// Armónico J4
	final static double J4 = -1.65597e-6;
	final static double ck2 = J2/2.0;
	final static double ck4 = -3.0*J4/8.0;
	final static double xj3 = J3;
	final static double qo = ae + (120.0/xkmper);
	final static double s = ae + (78/xkmper);
	final static double e6a = 1e-6;
	// Código de inialización del modelo Deep-Space
	final static int dpinit = 1;
	// Código secular del modelo Deep-Space
	final static int dpsec = 2;
	// Código periódico del modelo Deep-Space
	final static int dpper = 3;
	// Radio solar en kilómetros
	final static double sr = 696000.0;
	// Unidad Astronómica (distancia de la Tierra al Sol en kilómetros)
	final static double AU = 1.49597870e8;
	// Minutos por dia
	final static double xmnpda = 1440.0;
	// Segundos por dia
	final static double secday = 86400.0;
	// Rotaciones de la tierra por día sideral (no-constante)
	final static double omega_E = 1.00273790934;
	// Rotacion de la tierra, radianes por dia sideral
	final static double omega_ER = omega_E*(Math.PI*2.0);
	// Sqrt(ge) ER^3/min^2
	final static double xke = Math.sqrt(3600.0*ge/(xkmper*xkmper*xkmper));
	// (qo-s)^4 ER^4
	final static double qoms2t = ((qo-s)*(qo-s)*(qo-s)*(qo-s));
	// Twilight elevations
	final static double twilightCivil = Math.toRadians(-6);
	final static double twilightNautical = Math.toRadians(-12);
	final static double twilightAstronomical = Math.toRadians(-18);
	final static double mfactor = (Math.PI*2.0)*omega_E/secday;

}