import sgp.*;
import java.util.*;
import java.io.*;


public class PassFinderISS {

  public static void main(String[] args) {
    String nombreLocalidad = null;
    String latitud = null;
    String longitud = null;
    String altitud = null;
    String offsetUTC = null;
    double dias = 1.0;
    double maxelev_umbral = 10.0;

    PassView.actualizacionForzadaTLE(true);
    Date d = new Date();
    InformeHtml.cabecera(d.toString());
    // Porcesado de los argumentos
    if (args.length != 7){
      InformeHtml.error("Error: El número de argumentos no es adecuado.");
      InformeHtml.uso();
      InformeHtml.fin();
      System.exit(1);
    }
    else {
      nombreLocalidad = args[0];
      latitud = args[1];
      longitud = args[2];
      altitud = args[3];
      offsetUTC = args[4];
      try {
        dias = Double.parseDouble(args[5]);
      }
      catch (Exception e){
        InformeHtml.error("Error: El argumento Dias (6º) no es un número.");
        InformeHtml.uso();
        InformeHtml.fin();
        System.exit(1);
      }
      try {
        maxelev_umbral = Double.parseDouble(args[6]);
      }
      catch (Exception e){
        InformeHtml.error("Error: El argumento Maxima Elevación umbral (7º) no es un número.");
        InformeHtml.uso();
        InformeHtml.fin();
        System.exit(1);
      }
    }

    boolean status = true;
    status = PassView.setLugar(nombreLocalidad, latitud, longitud,altitud,offsetUTC);
    if (status)
      InformeHtml.localidad(PassView.getLugar());
    else {
      InformeHtml.error("Error en los datos de entrada para el lugar elegido.");
      InformeHtml.uso();
      InformeHtml.fin();
      System.exit(1);
    }

    status = PassView.setTLE();
    if (status){
      File tle = new File("stations.txt");
      if (tle.exists()){
        d = new Date(tle.lastModified());
      }
      else {
        d = new Date();
      }
      Satelite s = PassView.getSatelite();

      InformeHtml.satelite(s.nombre,s.tle.getLinea1TLE(),
                           s.tle.getLinea2TLE(), d.toString());
    }
    else {
      InformeHtml.error("    Archivo no disponible en disco. Falta conexión a Internet.");
      InformeHtml.uso();
      InformeHtml.fin();
      System.exit(1);
    }

    ArrayList avistamientos = PassView.getAvistamientos(dias);

    InformeHtml.avistamientos(avistamientos,args[5], args[6]);



  }
}