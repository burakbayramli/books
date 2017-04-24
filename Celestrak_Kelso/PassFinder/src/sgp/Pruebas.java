package sgp;

import java.io.*;
import java.util.*;

public class Pruebas
{
	public static void main(String[] args)
	{
		ArrayList satelites = new ArrayList();
		ArrayList lugares = new ArrayList();

		try
		{
			FactoriaSatelites.parseTLE("c:\\PassViewJava\\sgp\\visual.tle");
			int cuantosSat = FactoriaSatelites.howManySatelites();
			Satelite s;
			System.out.println("---------------------------------");
			System.out.println("Número de satélites: " + cuantosSat);
			System.out.println("---------------------------------");
			int j=0,numeroISS=0;
			while (FactoriaSatelites.hasMoreSatelites())
			{
				s = (Satelite)FactoriaSatelites.nextSatelite();
				if (s.nombre.startsWith("ISS (ZARYA)"))
					numeroISS = j;
				satelites.add(s);
				j++;
			}
			Satelite sat = (Satelite) satelites.get(numeroISS);
			// Ya tenemos la ISS seleccionada

			FactoriaLugares.parseLOC("c:\\PassViewJava\\sgp\\spain.loc");
			int cuantosLugares = FactoriaLugares.howManyLugares();
			Lugar l;
			System.out.println("Número de lugares: " + cuantosLugares);
			System.out.println("---------------------------------");
			while (FactoriaLugares.hasMoreLugares())
			{
				l = (Lugar)FactoriaLugares.nextLugar();
				lugares.add(l);
			}
			l = (Lugar)lugares.get(3);
			// Ya tenemos lugar (Murcia)

			System.out.println("Satélite: " + sat.nombre);
			System.out.println("Lugar: " + l.nombre);

			System.out.println("---------------------------------");

			// Miramos la hora local y universal
			MarcaDeTiempo ahora;
			Tiempo.setHorarioVerano(false);
			ahora = Tiempo.getCurrentLocalTime();
			System.out.println("Hora local:\n\t"+ahora);
			ahora = Tiempo.getCurrentUniversalTime(l.offsetUTC);
			System.out.println("Hora universal:\n\t"+ahora);
			double j1 = Tiempo.timeToJulianTime(ahora);
			System.out.println("Hora universal juliana:\n\t"+j1);
			Tiempo.julianTimeToTime(j1,ahora);
			System.out.println("Hora universal:\n\t"+ahora);
			System.out.println("---------------------------------");

			// Lo primero es calcular la posición solar
			Sol.calcularPosicion(ahora); // ahora tiene que ser universal

			System.out.println("Datos sobre el sol");
			System.out.println("\tpos[0]: " + Sol.pos[0]);
			System.out.println("\tpos[1]: " + Sol.pos[1]);
			System.out.println("\tpos[2]: " + Sol.pos[2]);
			System.out.println("\tpos[3]: " + Sol.pos[3]);
			System.out.println("\tLatitud: " + Sol.latitud);
			System.out.println("\tlongitud: " + Sol.longitud);
			System.out.println("\tAltitud: " + Sol.altitud);
			System.out.println("---------------------------------");

			// Ahora calculamos la posicion y velocidad del satélite
			sat.calcularVariables(ahora);
			System.out.println("Datos sobre el satélite "+sat.nombre+" :");

			System.out.println("\tpos[0]: "+sat.pos[0]);
			System.out.println("\tpos[1]: "+sat.pos[1]);
			System.out.println("\tpos[2]: "+sat.pos[2]);
			System.out.println("\tpos[3]: "+sat.pos[3]);
			System.out.println("\tvel[0]: "+sat.vel[0]);
			System.out.println("\tvel[1]: "+sat.vel[1]);
			System.out.println("\tvel[2]: "+sat.vel[2]);
			System.out.println("\tvel[3]: "+sat.vel[3]);
			System.out.println("\tLatitud: " + sat.latitud);
			System.out.println("\tLongitud: " + sat.longitud);
			System.out.println("\tAltitud: " + sat.altitud);
			System.out.println("---------------------------------");

			// Ahora tenemos que calcular el azimut y elevación del sol y satélite
			// respecto a la posición del observador

			l.calcularPosicionSol(ahora);
			System.out.println("Datos relativos al observador:");
			System.out.println("Sol:");
			System.out.println("\tAzimut sol:" + l.azimutSol);
			System.out.println("\tElevacion sol:" + l.elevacionSol);
			System.out.println("\tRango sol:" + l.rangoSol);
			System.out.println("\tRatioRango sol:" + l.ratioRangoSol);

			System.out.println(""+ sat.nombre);
			sat.calcularPosicionSatelite(l,ahora);
			System.out.println("\tAzimut sat:" + sat.azimut);
			System.out.println("\tElevacion sat:" + sat.elevacion);
			System.out.println("\tRango sat:" + sat.rango);
			System.out.println("\tRatioRango sat:" + sat.ratioRango);
			System.out.println("---------------------------------");

		}
		catch (FileNotFoundException e)
		{
			System.out.println("No se encuentra el fichero.");
		}






	}
}