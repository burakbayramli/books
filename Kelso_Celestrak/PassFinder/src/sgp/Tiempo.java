//
//
package sgp;

import java.util.*;

/**
 *	Clase que aglutina todas los métodos estáticos que tienen que ver con la
 *  manipulación y el control del tiempo.
 *
 *  @author Pedro J. Fernández
 *  @author Dr TS Kelso
 *  @version 1.0
 *
 */

public class Tiempo {
	/** Estamos o no estamos en horario de verano, que se aplica en
	 *  Europa adelantando una hora durante los meses estivales
	 *  Por defecto no estamos en horario de verano.
	 */
	private static boolean horarioVerano = false;

	/** Clase Calendar del que obtenemos la fecha y hora del sistema */
	private static Calendar cal = Calendario.getInstance();

	/** Marca de tiempo de uso temporal*/
	private static MarcaDeTiempo currentTime = new MarcaDeTiempo();

	/** Usada para cálculos internos */
	private static double ds50;


	/**
	 * Método que establece si el horario es de verano o no.
	 *
	 * @param b True -> Horario de Verano , False -> Horario de Invieno.
	 *
	 */
	public static void setHorarioVerano(boolean b)
	{
		horarioVerano = b;
	}


	/**
	 * Método que nos dice si el horario está establecido como de verano o no.
	 *
	 * @return True -> Horario de Verano , False -> Horario de Invieno.
	 *
	 */
	public static boolean getHorarioVerano()
	{
		return horarioVerano;
	}


	/**
	 * Método que devuelve el principio de la época, la fecha de donde se empieza a contar.
	 *
	 * @return Marca de tiempo ZERO, es decir, 00h.00m:00s del 1 de Enero de 1970
	 *
	 */
	public static MarcaDeTiempo zeroTime()
	{
		return new MarcaDeTiempo();
	}


	/**
	 * Método que devuelve la fecha y hora local actual directamente del reloj del sistema
	 * en forma de marca de tiempo.
	 *
	 * @return Marca de tiempo con la fecha y hora actuales.
	 *
	 */
	public static MarcaDeTiempo getCurrentLocalTime()
	{
		cal.setTimeInMillis(System.currentTimeMillis());
		currentTime.setValues(cal.get(Calendar.YEAR),
						 	  cal.get(Calendar.MONTH),
						 	  cal.get(Calendar.DATE),
						 	  cal.get(Calendar.HOUR_OF_DAY),
						 	  cal.get(Calendar.MINUTE),
						 	  cal.get(Calendar.SECOND),
						 	  cal.get(Calendar.MILLISECOND));

		return currentTime;
	}


	/**
	 * Método que devuelve la fecha y hora universal actual en una marca de tiempo
	 * aplicando el desplazamiento en horas que se pasa por parámetro sobre la hora
	 * local que se obtiene del sistema.
	 *
	 * @param offset Horas de diferencia entre la hora local y la universal (UTC).
	 * @return Marca de tiempo con la fecha y hora universal.
	 * @see #localToUniversalTime(MarcaDeTiempo,double)
	 */
	public static MarcaDeTiempo getCurrentUniversalTime(double offset)
	{
		getCurrentLocalTime();
		localToUniversalTime(currentTime,offset);
		return currentTime;
	}


	/**
	 * Método que devuelve la fecha y hora local actual directamente del reloj del sistema,
	 * pero en formato Juliano, es decir, como un número real.
	 *
	 * @return Real con la fecha y hora actuales en formato Juliano.
	 * @see #timeToJulianTime(MarcaDeTiempo)
	 */
	public static double getCurrentLocalJulianTime()
	{
		cal.setTimeInMillis(System.currentTimeMillis());
		currentTime.setValues(cal.get(Calendar.YEAR),
						 	  cal.get(Calendar.MONTH),
						 	  cal.get(Calendar.DATE),
						 	  cal.get(Calendar.HOUR_OF_DAY),
						 	  cal.get(Calendar.MINUTE),
						 	  cal.get(Calendar.SECOND),
						 	  cal.get(Calendar.MILLISECOND));

		return Tiempo.timeToJulianTime(currentTime);
	}


	/**
	 * Método que devuelve la fecha y hora universal actual en un real (Juliano)
	 * aplicando el desplazamiento en horas que se pasa por parámetro sobre la hora
	 * local que se obtiene del sistema.
	 *
	 * @param offset Horas de diferencia entre la hora local y la universal (UTC).
	 * @return Real con la fecha y hora universal en formato Juliano.
     * @see #timeToJulianTime(MarcaDeTiempo)
	 * @see #localToUniversalTime(MarcaDeTiempo,double)
	 */
	public static double getCurrentUniversalJulianTime(double offset)
	{
		getCurrentLocalTime();
		localToUniversalTime(currentTime,offset);
		return Tiempo.timeToJulianTime(currentTime);
	}


	/**
	 * Indica si el año pasado por parámetro es bisiesto o no.
	 *
     * @param year Año en cuestión.
	 * @return True -> Bisiesto,  False-> No bisiesto
	 *
	 */
	public static boolean bisiesto(int year)
	{
  		if ((year%4)==0)
			if ((year%100!=0)||(year%400==0))
    			return true;

    	return false;
	}




	/**
	 * Devuelve la fecha Juliana a partir de una marca de tiempo
	 *
	 * @param t Marca de tiempo a transformar.
	 * @return Real en el intervalo [0..1)
	 *
	 */
	public static double timeToJulianTime(MarcaDeTiempo t)
	{
	    return julianDateOfYear(t.yr)
				+ dayOfYear(t.yr,t.mo,t.dy)
	            + fractionOfDay(t.hr,t.mi,t.se,t.hu);
	}


	/**
	 * Obtiene la marca de tiempo correspondiente a una fecha Juliana.
	 *
	 * @param jd Fecha juliana a transformar.
	 * @param t Marca de tiempo a que se obtiene.
	 *
	 */
	public static void julianTimeToTime(double jd, MarcaDeTiempo t)
	{
		calendarDate(jd,t,3);
	  	timeOfDay(jd,t,3);
	}


	/**
	 * Corrige un posible fallo en el formato de la marca de tiempo, devolviendo
	 * una copia corregida. Puede servir para replicar marcas de tiempo.
	 *
	 *
	 * @param d Marca de tiempo a corregir.
	 * @return Marca de tiempo corregida.
	 *
	 */
	public static MarcaDeTiempo checkDate(MarcaDeTiempo d)
	{
	  double jt;
	  MarcaDeTiempo t = new MarcaDeTiempo();

	  jt = timeToJulianTime(d);
	  julianTimeToTime(jt,t);

	  return t;
	}


	/**
	 * Hace que la marca de tiempo (considerada la hora local) pase a ser
	 * la hora universal (UTC), según el desplazamiento horario pasado por
	 * parámetro.
	 * Se tiene en cuenta el cambio de hora en el caso de que nos encontráramos
	 * en horario de verano.
	 *
	 * @param t Marca de tiempo con la hora local.
	 * @param offsetUTC Desplazamiento horario en horas.
	 *
	 */
	public static void localToUniversalTime(MarcaDeTiempo t, double offsetUTC)
	{
		double jt = timeToJulianTime(t);

		jt = jt - (offsetUTC/24.0);
	  	if (horarioVerano)
	    	jt = jt - (1.0/24.0);

	  	julianTimeToTime(jt,t);
	}


	/**
	 * Hace que la marca de tiempo Juliana (considerada la hora local) pase a ser
	 * la hora universal (UTC), según el desplazamiento horario pasado por
	 * parámetro.
	 * Se tiene en cuenta el cambio de hora en el caso de que nos encontráramos
	 * en horario de verano.
	 *
	 * @param t Marca de tiempo en formato Juliano con la hora local.
	 * @param offsetUTC Desplazamiento horario en horas.
	 *
	 */
	public static double localToUniversalJulianTime(double jt, double offsetUTC)
	{
		double t;

		t = jt - (offsetUTC/24.0);
		if (horarioVerano)
	    	t = t - (1.0/24.0);

		return t;
	}


	/**
	 * Hace que la marca de tiempo (considerada la hora universal) pase a ser
	 * la hora local, según el desplazamiento horario pasado por parámetro.
	 * Se tiene en cuenta el cambio de hora en el caso de que nos encontráramos
	 * en horario de verano.
	 *
	 * @param ts Marca de tiempo con la hora universal.
	 * @param offsetUTC Desplazamiento horario en horas.
	 *
	 */
	public static void universalToLocalTime(MarcaDeTiempo ts, double offsetUTC)
	{
	  double jt;

	  jt = timeToJulianTime(ts);
	  jt = jt + (offsetUTC/24.0);
	  if (horarioVerano)
	    jt = jt + (1.0/24.0);

	  julianTimeToTime(jt,ts);
	}


	/**
	 * Hace que la marca de tiempo Juliana (considerada la hora universal)
	 * pase a ser la hora local, según el desplazamiento horario pasado por
	 * parámetro.
	 * Se tiene en cuenta el cambio de hora en el caso de que nos encontráramos
	 * en horario de verano.
	 *
	 * @param jt Marca de tiempo Juliana con la hora universal.
	 * @param offsetUTC Desplazamiento horario en horas.
	 *
	 */
	public static double universalToLocalJulianTime(double jt, double offsetUTC)
	{
	  jt = jt + (offsetUTC/24.0);
	  if (horarioVerano)
	    jt = jt + (1.0/24.0);

	  return jt;
	}


	/**
	 * Calcula la fecha Juliana en 1 de Enero a las 0 horas
	 * del año que se le pasa por parámetro.
	 *
	 * @param year Año en cuestión.
	 * @return Fecha y hora en formato Juliano del comienzo del año en cuenstión
	 *
	 */
	static double julianDateOfYear(double year)
	{
		long A, B;

		year = year - 1;
		// Formulas astronomicas para los calculos
		// obtenidas de Jean Meeus, paginas 23-25

		A = Math.round(Math.floor(year/100.0));
		B = Math.round(2.0 - A + Math.floor(A/4.0));

		return (Math.floor(365.25 * year)
	          + Math.floor(30.6001 * 14.0)
	          + 1720994.5 + B);
	}


	/**
	 * Calcula la fecha Juliana a partir de "epoch", es decir, un real que
	 * representa la linea de tiempo que va desde 1957 a 2056.
	 * Por tanto, este método sólo es válido durante estos años.
	 * Diseñado para que soporte Y2K (el efecto 2000).
	 *
	 * @param epoch Época
	 * @return Fecha y hora en formato Juliano del momento que señala "epoch".
	 *
	 */
	static double julianDateOfEpoch(double epoch)
	{
		double year, day;

		year = Math.floor(epoch*1e-3);
		if (year < 57)
			year = year + 2000;
		else
			year = year + 1900;
		day = ((epoch*1E-3)-Math.floor(epoch*1E-3))*1e3;

		return  (julianDateOfYear(year) + day);
	}


	/**
	 * Devuelve el número del día del año.
	 *
	 * @param yr Año
	 * @param mo Mes del año (0->Enero, 1->Febrero, ...)
	 * @param dy Día del mes
	 * @return Día del año.
	 *
	 */
	static int dayOfYear(int yr,int mo,int dy)
	{
		int[] days = new int[12];

		days[0] = 31;
		days[1] = 28;
		days[2] = 31;
		days[3] = 30;
		days[4] = 31;
		days[5] = 30;
		days[6] = 31;
		days[7] = 31;
		days[8] = 30;
		days[9] = 31;
		days[10] = 30;
		days[11] = 31;

		int day = 0;

		for (int i = 0;i<mo;i++)
			day = day + days[i];
		day = day + dy;
		if (bisiesto(yr) && (mo > 2))
			day = day + 1;

		return day;
	}


	/**
	 * A partir de una fecha Juliana obtenemos en la marca de tiempo <code>t</code>
	 * el año, mes y día, es decir, la fecha en el calendario.
	 * Las Formulas astronomicas para los cálculo han sido
	 * obtenidas de Jean Meeus, paginas 26-27
	 *
	 * @param jd Fecha Juliana
	 * @param t Marca de tiempo en donde se guardarán el año, mes y día que se obtienen.
	 * @param res Resolución de los cálculos, es decir, número de decimales a usar.
	 *
	 */
	static void calendarDate(double jd , MarcaDeTiempo t, double res)
	{
		long    Z;
		int month, day, year;
	    double A,B,C,D,E,F,alpha,factor;


		factor = 0.5/Cte.secday/Math.pow(10.0,res);
	  	F = (jd + 0.5) - Math.floor(jd + 0.5);
	  	if (F + factor >= 1.0)
		{
	    	jd = jd + factor;
	    	F = 0.0;
		}

	  	Z = Math.round(Math.floor(jd + 0.5));
	  	if (Z < 2299161)
	    	A = Z;
	  	else
	  	{
	    	alpha = Math.floor((Z - 1867216.25)/36524.25);
	    	A = (Z + 1.0 + alpha) - Math.floor(alpha/4.0);
	  	}

	  	B = A + 1524.0;
	  	C = Math.floor((B - 122.1)/365.25);
	  	D = Math.floor(365.25 * C);
	  	E = Math.floor((B - D)/30.6001);
		day = (int) Math.round(Math.floor(B - D - Math.floor(30.6001 * E) + F));
		if (E < 13.5)
			month = (int) Math.round(Math.rint(E - 1.0));
		else
			month = (int) Math.round(Math.rint(E - 13.0));

		if (month > 2.5)
			year = (int) Math.round(C - 4716.0);
		else
			year = (int) Math.round(C - 4715.0);

		t.yr = year;
		t.mo = month-1;
		t.dy = day;
	}


	/**
	 * Devuelve "epoch", la época a partir de una fecha Juliana.
	 *
	 * @param jd Fecha Juliana.
	 * @return "epoch", es decir, la época.
	 *
	 */
	static double epochTime(double jd)
	{
		int year, mo, dy;
		double yr,time;
	  	MarcaDeTiempo d = new MarcaDeTiempo();

		calendarDate(jd,d,3.0);
		year = d.yr;
		yr = (double)(d.yr%100);
		mo = d.mo;
		dy = d.dy;
		time = (jd + 0.5) - Math.floor(jd + 0.5);

		return (yr*1000 + dayOfYear(year,mo,dy) + time);
	}


	/**
	 * A partir de una fecha Juliana obtenemos en la marca de tiempo <code>t</code>
	 * las horas, minutos, segundos y centésimas de segundo.
	 * Las Formulas astronomicas para los cálculo han sido
	 * obtenidas de Jean Meeus, paginas 26-27
	 *
	 * @param jd Fecha Juliana
	 * @param t Marca de tiempo en donde se guardarán hora, minuto, segundo y centésimas de seg.
	 * @param res Resolución de los cálculos, es decir, número de decimales a usar.
	 *
	 */
	static void timeOfDay(double jd, MarcaDeTiempo t,double res)
	{
		int hr,mn,sec,hu;
		double factor,time,sc;

		res = Math.min(Math.max(0.0,res),3.0);
		factor = Math.pow(10.0,res);
		time = ((jd - 0.5)-Math.floor(jd - 0.5))*Cte.secday;
		time = Math.rint(time*factor)/factor;
		hr = (int) Math.round(Math.floor(time/3600.0));
		time = time - (3600.0*hr);
		if (hr == 24)
			hr = 0;
	  	mn = (int) Math.round(Math.floor(time/60.0));
	  	sc = time - (60.0*mn);

		hu = (int) Math.round(Math.rint((sc - Math.floor(sc))*1000));
		sec = (int) Math.round(Math.floor(sc));

		t.hr = hr;
		t.mi = mn;
		t.se = sec;
		t.hu = hu;
	}


	/**
	 * Devuelve la fraccion del día, es decir, un real en el intervalo [0..1)
	 *
	 * @param hr Hora del día
	 * @param mi Minuto de la hora
	 * @param se Segundo del minuto
	 * @param hu Centésimas de segundo
	 * @return Real en el intervalo [0..1)
	 *
	 */
	static double fractionOfDay(int hr,int mi,int se,int hu)
	{
		return ((hr + (mi + (se + hu/1000.0)/60.0)/60.0)/24.0);
	}


	/**
     * Función matemática "ThetaG" usada para los cálculos.
	 * Obtenido de "The 1992 Astronomical Almanac", pagina B6.
	 * Diseñada para que soporte Y2K (el efecto 2000), y sólo es válida
	 * entre los años 1957 y 2056.
	 *
     *
	 * @param epoch La época
	 * @return El resultado de esta función
	 */
	static double thetaG(double epoch)
	{
		double year,day,UT,jd,TU,GMST,result;

		year = Math.floor(epoch*1e-3);
		if (year < 57)
		    year = year + 2000;
		else
		    year = year + 1900;
		day = ((epoch*1e-3)-Math.floor(epoch*1e-3))*1e3;
		UT  = (day)-Math.floor(day);
		day = Math.floor(day);
		jd = julianDateOfYear(year) + day;
		TU = (jd - 2451545.0)/36525;
		GMST = 24110.54841 + (TU * (8640184.812866 + (TU * (0.093104 - (TU * 6.2e-6)))));
		GMST = FuncionesMatematicas.modulus(GMST + (Cte.secday*Cte.omega_E*UT), Cte.secday);
		result = (Math.PI*2.0) * GMST/Cte.secday;
		ds50 = jd - 2433281.5 + UT;

		/* Esta línea se encontraba comentada en el código original. No se si hace falta. */
		/* result = FuncionesMatematicas.modulus(6.3003880987*ds50 + 1.72944494,(Math.PI*2)); */

		return result;
	}


	/**
	 * Función matemática "ThetaG_JD" usada para los cálculos.
	 * Obtenido de "The 1992 Astronomical Almanac", pagina B6.
	 *
	 * @param jd Marca de tiempo en formato Juliano.
	 * @return El resultado de esta función
	 */
	static double thetaG_JD(double jd)
	{

		double  UT,TU,GMST;

		UT = (jd + 0.5)-Math.floor(jd + 0.5);
		jd = jd - UT;
		TU = (jd - 2451545.0)/36525;
		GMST = 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2e-6));
		GMST = FuncionesMatematicas.modulus(GMST + Cte.secday*Cte.omega_E*UT,Cte.secday);
		return ((Math.PI*2.0) * GMST/Cte.secday);
	}


	/**
	 * Función matemática "DeltaET" usada para los cálculos.
	 * Valores determinados usando datos comprendidos entre los años
	 * 1950 y 1991 en el "The 1990 Astronomical Almanac."
	 *
	 *
	 * @param year Año
	 * @return El resultado de esta función
	 */
	static double deltaET(double year)
	{

		return (26.465 + 0.747622*(year - 1950)
	             + 1.886913*Math.sin((Math.PI*2.0)*(year - 1975.0)/33.0));

	}

}


