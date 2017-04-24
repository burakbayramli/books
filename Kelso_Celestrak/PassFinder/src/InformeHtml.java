import sgp.*;
import java.util.*;

public class InformeHtml {

  public static void cabecera(String fecha){
    System.out.print("<html>\n");
    System.out.print("<head> <title>Informe ISS - "+fecha+"</title></head>\n");
    System.out.print("<body>\n");

    System.out.print("<h1>Informe de avistamientos de la Estación Espacial Internacional (ISS)</h1>\n");
    System.out.print("Fecha: "+fecha+"\n");
    System.out.print("<hr>\n");

  }

  public static void localidad(Lugar loc){
    System.out.print("<h2>Datos de la localidad de "+loc.nombre+"</h2>\n");
    System.out.print("<dir>\n");
    System.out.print("Latitud  = "+loc.latitud+"º N<br>\n");
    System.out.print("Longitud = "+loc.longitud+"º E<br>\n");
    System.out.print("Altitud  = "+loc.altitud+" m<br>\n");
    System.out.print("Diferencia horaria con la hora universal UTC = "+loc.offsetUTC+" h.<br>\n");
    System.out.print("</dir>\n");
    System.out.print("<hr>\n");
  }

  public static void satelite(String nombre, String linea1, String linea2, String fecha){
    System.out.print("<h2>Datos del satélite "+nombre+"</h2>\n");
    System.out.print("<dir> \n");
    System.out.print("Procedencia de los datos: http://celestrak.com/NORAD/elements/stations.txt<br>\n");
    System.out.print("Fecha de los datos: "+fecha+"<br>\n");
    System.out.print("TLE:<br><dir><font face=courier size=2>");
    System.out.print(linea1+"<br>");
    System.out.print(linea2+"<br></font></dir>");
    System.out.print("</dir>\n");
    System.out.print("<hr>\n");
  }

  public static void avistamientos(ArrayList a, String dias, String elevacion_umbral){
    double elev_max = 10.0;
    try{
      elev_max = Double.parseDouble(elevacion_umbral);
    }catch(Exception e){
      elev_max = 10.0;
    }

    System.out.print("<h2>Próximos avistamientos del satelite ISS en los siguentes "+dias+" días</h2>\n");
    System.out.print("<dir>\n");
    System.out.print("Sólo se muestran los que superen los "+elev_max+"º de elevación máxima.<br> \n");
    System.out.print("<br>\n");
    System.out.print("<table border=3>\n");
    System.out.print("<tr bgcolor=#bbbbbb><td><b>Fecha</b></td><td><b>Inicio</b></td><td><b>Permanece iluminado</b></td><td><b>Fin</b></td><td><b>Max. Elev.</b></td></tr>\n");

    if (a != null) {
      if (a.size() > 0) {
        Iterator it = a.iterator();
        while (it.hasNext()) {
          Avistamiento avist = (Avistamiento) it.next();
          // Solo imprimimos aquellas que pasen de un umbral de máxima elevación mínima
          if (avist.maximaElevacion.elevacion > elev_max)
            System.out.println(avist.toHtml());
        }
        System.out.print("</table>\n");
        System.out.print("<br>Horas en formato 24H.<br>\n");
      }
      else {
        System.out.print("</table><br>No se encontraron avistamientos.<br>\n");
      }
    }
    else {
      System.out.print("</table><br>\n");
      error("Se produjo un error en el calculo.");
      System.exit(1);
    }


  }


  public static void fin(){
    System.out.print("</body>\n");
    System.out.print("</html>\n");
  }

  public static void error(String e){
    System.out.print("<font size=4 color=#ff0000>\n");
    System.out.print("<b>"+e+"</b><br>\n");
    System.out.print("</font>\n");
  }

  public static void uso(){
    System.out.print("Uso: PassFinderISS nombre_localidad latitud longitud altitud offsetUTC dias elev_max");
  }


}