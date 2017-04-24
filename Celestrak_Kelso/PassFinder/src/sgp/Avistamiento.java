package sgp;

import java.text.*;

public class Avistamiento {
  public Satelite
      inicio=null,
      inicioLuz=null,
      fin=null,
      finLuz=null,
      maximaElevacion=null;
  public double offsetUTC = 0.0;


  private String printDouble(String pattern, double value ) {
     DecimalFormat myFormatter = new DecimalFormat(pattern);
     return myFormatter.format(value);
  }

  private String dd(int n){
    String s = "" + n;
    if (s.length()==1)
      s = "0"+s;
    return s;
  }

  public String toHtml(){
    Tiempo.universalToLocalTime(inicio.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(inicioLuz.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(fin.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(finLuz.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(maximaElevacion.marcaDeTiempo,offsetUTC);

    String fecha = "" + dd(inicio.marcaDeTiempo.dy) +
        "/" + dd((inicio.marcaDeTiempo.mo+1)) +
        "/" + inicio.marcaDeTiempo.yr;
    String horaInicio = "" + dd(inicio.marcaDeTiempo.hr) +
        ":" + dd(inicio.marcaDeTiempo.mi);
    String horaInicioIlum = "" + dd(inicioLuz.marcaDeTiempo.hr) +
        ":" + dd(inicioLuz.marcaDeTiempo.mi);
    String horaFin = "" + dd(fin.marcaDeTiempo.hr) +
        ":" + dd(fin.marcaDeTiempo.mi);
    String horaFinIlum = "" + dd(finLuz.marcaDeTiempo.hr) +
        ":" + dd(finLuz.marcaDeTiempo.mi);
    String maxElev = printDouble("##.#", maximaElevacion.elevacion) +
        " " + maximaElevacion.puntoCardinal;

    return "<tr><td>"+ fecha + "</td><td>" + horaInicio +" "+ inicio.puntoCardinal +
        "</td><td>" + horaInicioIlum +" "+ inicioLuz.puntoCardinal +
        " - " + horaFinIlum +" "+ finLuz.puntoCardinal + "</td><td>"
        + horaFin +" "+fin.puntoCardinal +
        "</td><td>" + maxElev +"</td></tr>";

  }


  public String toString(){
    Tiempo.universalToLocalTime(inicio.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(inicioLuz.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(fin.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(finLuz.marcaDeTiempo,offsetUTC);
    Tiempo.universalToLocalTime(maximaElevacion.marcaDeTiempo,offsetUTC);

    String fecha = "" + dd(inicio.marcaDeTiempo.dy) +
        "/" + dd((inicio.marcaDeTiempo.mo+1)) +
        "/" + inicio.marcaDeTiempo.yr;
    String horaInicio = "" + dd(inicio.marcaDeTiempo.hr) +
        ":" + dd(inicio.marcaDeTiempo.mi);
    String horaInicioIlum = "" + dd(inicioLuz.marcaDeTiempo.hr) +
        ":" + dd(inicioLuz.marcaDeTiempo.mi);
    String horaFin = "" + dd(fin.marcaDeTiempo.hr) +
        ":" + dd(fin.marcaDeTiempo.mi);
    String horaFinIlum = "" + dd(finLuz.marcaDeTiempo.hr) +
        ":" + dd(finLuz.marcaDeTiempo.mi);
    String maxElev = printDouble("##.#", maximaElevacion.elevacion) +
        " " + maximaElevacion.puntoCardinal;


    return fecha + " " + horaInicio +" "+ inicio.puntoCardinal +
        " (" + horaInicioIlum +" "+ inicioLuz.puntoCardinal +
        " - " + horaFinIlum +" "+ finLuz.puntoCardinal + ") "
        + horaFin +" "+fin.puntoCardinal +
        "  " + maxElev ;
  }
}