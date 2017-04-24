// Clase estática que permite recuperar los lugares a partir de un fichero .loc
package sgp;

import java.util.*;
import java.io.*;

class FactoriaLugares{
	private static ArrayList lugares = new ArrayList();

	static void parseLOC(String archivoLugares) throws FileNotFoundException
	{
		FileReader fr = new FileReader(archivoLugares);
		BufferedReader br = new BufferedReader(fr);
		String linea, nombre;
		double latitud,longitud,altitud,offset;

		try
		{
			do
			{
				linea = br.readLine();
				if (linea != null)
				{
					StringTokenizer st = new StringTokenizer(linea,":",false);
					if (st.countTokens()==5) {
						nombre = st.nextToken();
						latitud = Double.parseDouble(st.nextToken());
						longitud = Double.parseDouble(st.nextToken());
						altitud = Double.parseDouble(st.nextToken());
						offset = Double.parseDouble(st.nextToken());
						Lugar lugar = new Lugar(nombre,latitud,longitud,altitud,offset);
						lugar.setFicheroLOC(archivoLugares);
						lugares.add(lugar);
					}
				}
				else break;
			}
			while (true);
			br.close();
			fr.close();
			Object[] satArray = lugares.toArray();
			Arrays.sort(satArray);
			lugares.clear();
			for (int i=0;i<satArray.length;i++)
				lugares.add(satArray[i]);

		}
		catch (IOException e)
		{
			System.err.println("error: Leyendo archivo LOC : " + e );
		}
		catch (NumberFormatException e)
		{
			System.err.println("error: Leyendo en un campo del archivo LOC : " + e );
		}
	}


	static boolean hasMoreLugares()
	{
		if (howManyLugares() == 0)
			return false;
		else
			return true;
	}

	static Lugar nextLugar()
	{
		try
		{
			Lugar loc = (Lugar) lugares.get(0);
			lugares.remove(0);

			return loc;
		}
		catch (IndexOutOfBoundsException e)
		{
			return null;
		}
	}

	static int howManyLugares()
	{
		return lugares.size();
	}

	static void addLugarFicheroLOC(String fichero, Lugar lugar)
	{

		try
		{
			File f = new File(fichero);
			if (!f.exists())
				f.createNewFile();

			FileReader fr = new FileReader(fichero);
			BufferedReader br = new BufferedReader(fr);

			String linea;
			ArrayList lineas = new ArrayList();

			do
			{
				linea = br.readLine();
				if (linea != null)
					lineas.add(linea);
				else
					break;
			}
			while(true);
			br.close();
			fr.close();
			linea = "" + lugar.nombre+":"+lugar.latitud+":"+lugar.longitud+":"+lugar.altitud+":"+lugar.offsetUTC;
			lineas.add(linea);

			FileOutputStream fos = new FileOutputStream(fichero);
			PrintWriter pw = new PrintWriter(fos);

			for (int i = 0; i < lineas.size(); i++)
				pw.println((String) lineas.get(i));
			pw.flush();
			pw.close();
			fos.close();

		}

		catch(FileNotFoundException e)
		{

		}
		catch(IOException e)
		{

		}

	}

	static void eliminarLugarFicheroLOC(Lugar lugar)
	{
		String fichero = lugar.getFicheroLOC();

		try
		{
			FileReader fr = new FileReader(fichero);
			BufferedReader br = new BufferedReader(fr);

			String linea;
			ArrayList lineas = new ArrayList();

			do
			{
				linea = br.readLine();
				if (linea != null)
				{
					if (!linea.substring(0,lugar.nombre.length()).equals(lugar.nombre))
						lineas.add(linea);
				}
				else
					break;
			}
			while(true);
			br.close();
			fr.close();

			FileOutputStream fos = new FileOutputStream(fichero);
			PrintWriter pw = new PrintWriter(fos);

			for (int i = 0; i < lineas.size(); i++)
				pw.println((String) lineas.get(i));
			pw.flush();
			pw.close();
			fos.close();

		}

		catch(FileNotFoundException e)
		{
			System.err.println("error: eliminando " + e );

		}
		catch(IOException e)
		{
			System.err.println("error: eliminando 2 : " + e );

		}


	}



}
