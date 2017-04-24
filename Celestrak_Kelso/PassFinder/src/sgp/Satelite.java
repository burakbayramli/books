// Clase que representa a un satélite
package sgp;

public class Satelite implements Comparable, Cloneable {
  public String nombre;
  public double azimut, elevacion, rango, ratioRango;
  public double latitud, longitud, altitud;
  public boolean visible, eclipsado;
  public String puntoCardinal;
  public MarcaDeTiempo marcaDeTiempo;



  public TLE tle;
  MarcaDeTiempo tiempo = new MarcaDeTiempo();
  double tiempoJuliano = 0;
  int iexp, ibexp, ideep, iflag;
  double epoch, julian_epoch, xndt2o, xndd6o, bstar;
  double xincl, xnodeo, eo, omegao, xmo, xno;
  String catnr, elset;
  double[] pos = new double[4];
  double[] vel = new double[4];
  private double[] range = new double[4];
  private double[] rgvel = new double[4];
  double depth;

  double a1 = 0, a3ovk2 = 0, ao = 0;
  double aodp = 0, aycof = 0, betao = 0;
  double betao2 = 0, c1 = 0, c1sq = 0;
  double c2 = 0, c3 = 0, c4 = 0;
  double c5 = 0, coef = 0, coef1 = 0;
  double cosio = 0, d2 = 0, d3 = 0;
  double d4 = 0, del1 = 0, delmo = 0;
  double delo = 0, eeta = 0, eosq = 0;
  double eta = 0, etasq = 0, xnodp = 0;
  double omgcof = 0, omgdot = 0, perige = 0;
  double pinvsq = 0, psisq = 0, qoms24 = 0;
  double s4 = 0, sinio = 0, sinmo = 0;
  double t2cof = 0, t3cof = 0, t4cof = 0;
  double t5cof = 0, theta2 = 0;
  double theta4 = 0, tsi = 0, x1m5th = 0;
  double x1mth2 = 0, x3thm1 = 0, x7thm1 = 0;
  double xhdot1 = 0, xlcof = 0, xmcof = 0;
  double xmdot = 0, xnodcf = 0, xnodot = 0;
  int isimp = 0;

  // Constructor
  Satelite(TLE tle) {
    this.tle = tle;
    nombre = tle.getNombreSatelite();
    // 1ª línea TLE
    catnr = tle.getCatnr();
    epoch = tle.getEpoch();
    julian_epoch = Tiempo.julianDateOfEpoch(epoch);
    xndt2o = tle.getXndt2o();
    xndd6o = tle.getXndd6o();
    iexp = tle.getIexp();
    bstar = tle.getBstar();
    ibexp = tle.getIbexp();
    elset = tle.getElset();

    //2ª línea TLE
    xincl = tle.getXincl();
    xnodeo = tle.getXnodeo();
    eo = tle.getEo();
    omegao = tle.getOmegao();
    xmo = tle.getXmo();
    xno = tle.getXno();

    // Conversión a unidades adecuadas
    xndd6o = xndd6o * Math.pow(10.0, iexp);
    bstar = (bstar * Math.pow(10.0, ibexp)) / Cte.ae;
    xnodeo = Math.toRadians(xnodeo);
    omegao = Math.toRadians(omegao);
    xmo = Math.toRadians(xmo);
    xincl = Math.toRadians(xincl);
    xno = (xno * (Math.PI * 2.0)) / Cte.xmnpda;
    xndt2o = (xndt2o * (Math.PI * 2.0)) / (Cte.xmnpda * Cte.xmnpda);
    xndd6o = (xndd6o * (Math.PI * 2.0)) / (Cte.xmnpda * Cte.xmnpda * Cte.xmnpda);

    // Determinación del modelo Deep-Space a usar
    double a1, a0, del1, del0, xnodp, temp;

    a1 = Math.pow(Cte.xke / xno, Cte.tothrd);
    temp = 1.5 * Cte.ck2 * ( (3.0 * Math.pow(Math.cos(xincl), 2.0)) - 1.0) /
        Math.pow(1.0 - (eo * eo), 1.5);
    del1 = temp / (a1 * a1);
    a0 = a1 *
        (1.0 - del1 * (0.5 * Cte.tothrd + del1 * (1.0 + 134.0 / 81.0 * del1)));
    del0 = temp / (a0 * a0);
    xnodp = xno / (1.0 + del0);
    if ( ( (Math.PI * 2.0) / xnodp) >= 255)
      ideep = 1;
    else
      ideep = 0;
    iflag = 1;
  }

  void calcularVariables(MarcaDeTiempo t) {
    marcaDeTiempo = t;
    calcularVariables(Tiempo.timeToJulianTime(t));
  }

  void calcularVariables(double tj) {
    tiempoJuliano = tj;

    Tiempo.julianTimeToTime(tj, tiempo);
    marcaDeTiempo = tiempo;
    SGP.SGP(this);
    setEclipsacionSatelite();
    calcularLatLonAlt(tj);
  }

  void setEclipsacionSatelite() {
    double sd_sun, sd_earth;
    double delta;
    double[] rho = new double[4];
    double[] earth = new double[4];

    // Determina eclipse parcial
    sd_earth = Math.asin(Cte.xkmper / pos[3]);
    FuncionesMatematicas.subVectores(Sol.pos, pos, rho);
    sd_sun = Math.asin(Cte.sr / rho[3]);
    FuncionesMatematicas.multEscalarVector(pos, -1.0, earth);
    delta = FuncionesMatematicas.angulo(Sol.pos, earth);
    depth = sd_earth - sd_sun - delta;
    if (sd_earth < sd_sun)
      eclipsado = false;
    else
    if (depth >= 0)
      eclipsado = true;
    else
      eclipsado = false;

  }

  void calcularPosicionSatelite(Lugar obs, MarcaDeTiempo t) {
    calcularPosicionSatelite(obs, Tiempo.timeToJulianTime(t));
  }

  void calcularPosicionSatelite(Lugar obs, double jt) {
    int i;
    double sin_lat, cos_lat, sin_theta, cos_theta;
    double el, azim, lat, theta;
    double top_s, top_e, top_z;

    obs.calcularVariables(jt);
    for (int n = 0; n < 3; n++) {
      range[n] = pos[n] - obs.pos[n];
      rgvel[n] = vel[n] - obs.vel[n];
    }
    FuncionesMatematicas.magnitud(range);

    lat = obs.polares[0];
    theta = obs.polares[3];
    sin_lat = Math.sin(lat);
    cos_lat = Math.cos(lat);
    sin_theta = Math.sin(theta);
    cos_theta = Math.cos(theta);
    top_s = sin_lat * cos_theta * range[0]
        + sin_lat * sin_theta * range[1]
        - cos_lat * range[2];
    top_e = -sin_theta * range[0]
        + cos_theta * range[1];
    top_z = cos_lat * cos_theta * range[0]
        + cos_lat * sin_theta * range[1]
        + sin_lat * range[2];
    azim = Math.atan( -top_e / top_s); //Azimuth
    if (top_s > 0)
      azim = azim + Math.PI;
    if (azim < 0)
      azim = azim + (Math.PI * 2.0);
    el = Math.asin(top_z / range[3]);
    azimut = Math.toDegrees(azim); //Azimuth (radians to degrees)
    traduceAzimut();
    elevacion = el; //Elevation (radians)
    rango = range[3]; //Range (kilometers)
    ratioRango = FuncionesMatematicas.dot(range, rgvel) / range[3]; //Range Rate (kilometers/second)
    // Corrections for atmospheric refraction }
    // Reference:  Astronomical Algorithms by Jean Meeus, pp. 101-104 }
    // Note:  Correction is meaningless when apparent elevation is below horizon }
    elevacion = elevacion +
        Math.toRadians( (1.02 /
                         Math.tan(Math.toRadians(Math.toDegrees(el) +
                                                 10.3 /
                                                 (Math.toDegrees(el) + 5.11)))) /
                       60.0);
    if (elevacion < 0)
      elevacion = el; //Reset to true elevation
    elevacion = Math.toDegrees(elevacion);

    visible = (! (eclipsado) && (obs.esDeNoche()) && (elevacion > 0));
  }

  void calcularLatLonAlt(double time) {
    double alt, lon, lat;
    double theta, r, e2, phi, c;

    theta = FuncionesMatematicas.acTan(pos[1], pos[0]);
    lon = FuncionesMatematicas.modulus(theta - Tiempo.thetaG_JD(time),
                                       Math.PI * 2.0);
    r = Math.sqrt( (pos[0] * pos[0]) + (pos[1] * pos[1]));
    e2 = Cte.f * (2.0 - Cte.f);
    lat = FuncionesMatematicas.acTan(pos[2], r);
    do {
      phi = lat;
      c = 1.0 / Math.sqrt(1.0 - e2 * Math.pow(Math.sin(phi), 2.0));
      lat = FuncionesMatematicas.acTan(pos[2] +
                                       Cte.xkmper * c * e2 * Math.sin(phi), r);
    }
    while (! (Math.abs(lat - phi) < 1e-10));

    alt = r / Math.cos(lat) - Cte.xkmper * c;

    latitud = Math.toDegrees(lat);
    longitud = Math.toDegrees(lon);
    altitud = alt;
    if (longitud >= 180)
      longitud = longitud - 360;
  }

  void traduceAzimut() {
    if ( (0 <= azimut) && (azimut < 22.5))
      puntoCardinal = "N ";
    else if ( (22.5 <= azimut) && (azimut < 67.5))
      puntoCardinal = "NE";
    else if ( (67.5 <= azimut) && (azimut < 112.5))
      puntoCardinal = "E ";
    else if ( (112.5 <= azimut) && (azimut < 157.5))
      puntoCardinal = "SE";
    else if ( (157.5 <= azimut) && (azimut < 202.5))
      puntoCardinal = "S ";
    else if ( (202.5 <= azimut) && (azimut < 247.5))
      puntoCardinal = "SW";
    else if ( (247.5 <= azimut) && (azimut < 292.5))
      puntoCardinal = "W ";
    else if ( (292.5 <= azimut) && (azimut < 337.5))
      puntoCardinal = "NW";
    else if ( (337.5 <= azimut) && (azimut < 360))
      puntoCardinal = "N ";
  }

  public int compareTo(Object o) {
    Satelite s = (Satelite) o;
    return nombre.compareTo(s.nombre);
  }

  public Object clone() throws CloneNotSupportedException {
    Satelite s = (Satelite) super.clone();
    s.marcaDeTiempo = new MarcaDeTiempo(marcaDeTiempo.yr,
                                        marcaDeTiempo.mo,
                                        marcaDeTiempo.dy,
                                        marcaDeTiempo.hr,
                                        marcaDeTiempo.mi,
                                        marcaDeTiempo.se,
                                        marcaDeTiempo.hu);
    return s;
  }

}