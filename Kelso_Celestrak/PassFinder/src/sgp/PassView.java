package sgp;

import java.io.*;
import java.util.*;
import java.net.*;

public class PassView {

  private static Satelite satelite = null;
  private static Lugar lugar = null;
  private static boolean forzarActualizacionTLE = false;

  public static boolean setLugar(String nombre, String lat, String lon, String alt,
                                 String diffHoraria) {
    double latitud, longitud, altitud, offset;
    try {
      latitud = Double.parseDouble(lat);
      longitud = Double.parseDouble(lon);
      altitud = Double.parseDouble(alt);
      offset = Double.parseDouble(diffHoraria);
      lugar = new Lugar(nombre, latitud, longitud, altitud, offset);
    }
    catch (Exception e) {
      return false;
    }
    return true;
  }

  public static boolean setTLE() {
    File f = new File("stations.txt");
    try {
      InputStreamReader isr = null;
      FileReader fr = null; ;
      BufferedReader br = null; ;
      if (!f.exists() || forzarActualizacionTLE) {
        //Se usa el fichero de internet
        URL url = new URL("http://celestrak.com/NORAD/elements/stations.txt");
        URLConnection c = url.openConnection();
        isr = new InputStreamReader(c.getInputStream());
        br = new BufferedReader(isr);
      }
      else {
        // Si hay fichero de satelite en el disco, no se usa internet
        fr = new FileReader("stations.txt");
        br = new BufferedReader(fr);
      }
      String nombre, linea1, linea2;
      nombre = br.readLine();
      if (nombre == null)
        return false;
      linea1 = br.readLine();
      if (linea1 == null)
        return false;
      linea2 = br.readLine();
      if (linea2 == null)
        return false;
      satelite = new Satelite(new TLE(nombre, linea1, linea2));

      if (!f.exists() || forzarActualizacionTLE) {
        if (br != null)
          br.close();
        if (isr != null)
          isr.close();

        f.createNewFile();
        FileOutputStream fos = new FileOutputStream(f);
        PrintWriter pw = new PrintWriter(fos);

        pw.println(nombre);
        pw.println(linea1);
        pw.println(linea2);

        pw.close();

      }
      else {
        if (br != null)
          br.close();
        if (fr != null)
          fr.close();

      }

    }
    catch (Exception e) {
      return false;
    }
    return true;

  }

  public static Lugar getLugar(){
    return lugar;
  }


  public static Satelite getSatelite() {
    if (lugar == null)
      return null;
    if (satelite == null)
      return null;

    MarcaDeTiempo ahora;
    ahora = Tiempo.getCurrentUniversalTime(lugar.offsetUTC);
    Sol.calcularPosicion(ahora);
    lugar.calcularPosicionSol(ahora);
    satelite.calcularVariables(ahora);
    satelite.calcularPosicionSatelite(lugar, ahora);

    return satelite;

  }

  public static Satelite getSatelite(double tiempo) {
    if (lugar == null)
      return null;
    if (satelite == null)
      return null;

    MarcaDeTiempo mt = new MarcaDeTiempo();

    Sol.calcularPosicion(tiempo);
    lugar.calcularPosicionSol(tiempo);
    satelite.calcularVariables(tiempo);
    satelite.calcularPosicionSatelite(lugar, tiempo);

    return satelite;

  }

  public static ArrayList getAvistamientos(double dias) {
    double step = 1.15740722960440E-5 * 30.0; // 30 segundos
    double tiempo = Tiempo.getCurrentUniversalJulianTime(lugar.offsetUTC);
    double limite = tiempo + (step * 2.0 * 60.0 * 24.0 * dias);
    ArrayList avistamientos = new ArrayList();

    Satelite sat = null;

    for (; ; ) {
      try {

        // Mientras no sea visible, avanzar en el tiempo
        do {
          tiempo = tiempo + step;
          sat = getSatelite(tiempo);
        }
        while ( (!sat.visible) && (tiempo < limite));

        // Comprobamos si hemos llegado al limite
        if (tiempo >= limite)
          return avistamientos; // No buscamos más.

        Avistamiento a = new Avistamiento();
        // Guardamos el inicio de la iluminacion
        a.inicioLuz = (Satelite) sat.clone();
        // Guardamos la marca de tiempo
        double temp = tiempo;

        // Retrocedemos en el tiempo en busca del horizonte
        do {
          tiempo = tiempo - step;
          sat = getSatelite(tiempo);
        }
        while ( (sat.elevacion > 0));
        // Adelantamos un paso
        tiempo = tiempo + step;
        sat = getSatelite(tiempo);
        // Guardamos la aparición por el horizonte
        a.inicio = (Satelite) sat.clone();
        // Recuperamos la marca de tiempo guardada
        tiempo = temp;
        sat = getSatelite(tiempo);

        double elevacionMaxima = sat.elevacion;
        // Mientras sea visible, avanzar en el tiempo
        do {
          tiempo = tiempo + step;
          sat = getSatelite(tiempo);
          if (elevacionMaxima < sat.elevacion) {
            elevacionMaxima = sat.elevacion;
          }
          else {
            // Empieza a decrecer
            if (a.maximaElevacion == null) {
              tiempo = tiempo - step;
              sat = getSatelite(tiempo);
              a.maximaElevacion = (Satelite) sat.clone();
              tiempo = tiempo + step;
            }
          }
        }
        while (sat.visible);
        // Por si no ha llegado a bajar la elevacion
        if (a.maximaElevacion == null) {
          tiempo = tiempo - step;
          sat = getSatelite(tiempo);
          a.maximaElevacion = (Satelite) sat.clone();
          tiempo = tiempo + step;
        }

        // Guardamos el fin de la iluminación
        a.finLuz = (Satelite) sat.clone();
        // Mientras esté sobre el horizonte, avanzar en el tiempo
        while (sat.elevacion > 0) {
          tiempo = tiempo + step;
          sat = getSatelite(tiempo);
        }
        a.fin = (Satelite) sat.clone();
        a.offsetUTC = lugar.offsetUTC;
        avistamientos.add(a);
      }
      catch (Exception e) {
        System.out.println("Error, no se pudo copiar satelite.");
      }

    }
  }

  public static void actualizacionForzadaTLE(boolean b){
    forzarActualizacionTLE = b;
  }


}