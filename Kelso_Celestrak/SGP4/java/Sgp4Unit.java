package sgp4v;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.Vector;

import javax.vecmath.Vector3d;

/**
 * ----------------------------------------------------------------
 * 
 * sgp4unit
 * 
 * this file contains the sgp4 procedures. the code was originally released in
 * the 1980 paper. in 1997 and 1998, the updated and combined code (sgp4 and
 * sdp4) was released by nasa on the internet.
 * seawifs.gsfc.nasa.gov/~seawifsp/src/bobdays/
 * 
 * companion code for fundamentals of astrodyanmics and applications 2004 by
 * david vallado
 * 
 * (w) 719-573-2600, email dvallado@agi.com
 * 
 * current : 26 jul 05 david vallado fixes for paper note that each fix is
 * preceded by a comment with "sgp4fix" and an explanation of what was changed
 * changes : 10 aug 04 david vallado 2nd printing baseline working 14 may 01
 * david vallado 2nd edition baseline 97 nasa internet version 80 norad original
 * baseline ----------------------------------------------------------------
 * 
 * Conversion to Java by Joe Coughlin at joe.coughlin@mslco.com
 */

public class Sgp4Unit {

    public static final TimeZone GMT_TZ = TimeZone.getTimeZone("GMT");

    /**
     * Main to test the implementation
     * 
     * @param args
     */
    public static void main(String[] args) {

        double radiusearthkm = 6378.135;
        double vkmpersec = radiusearthkm * 0.0743669161331734132 / 60.0;

        String card1 = "1 02608U 66110A   04029.48034756  .00000000  00000-0  10000-3 0  3978";
        String card2 = "2 02608  11.5047 324.1896 0006312 138.8745 221.2273  1.00350455 52781";
        int startYr = 04;
        double startDay = 029.48034756;
        int stopYr = 04;
        double stopDay = 030.48034756;
        double step = 60.00;  // minutes

        long time1 = System.currentTimeMillis(); ;

        Sgp4Data data = null;
        try {

            Sgp4Unit sgp4 = new Sgp4Unit();
            Vector results = sgp4.runSgp4(card1, card2, startYr, startDay,
                    stopYr, stopDay, step);
            for (int i = 0; i < results.size(); i++) {
                data = (Sgp4Data) results.elementAt(i);

                System.out.println("x " + data.getX() * radiusearthkm + " y "
                     + data.getY() * radiusearthkm + " z " + data.getZ()
                     * radiusearthkm + " vx " + data.getXdot() * vkmpersec
                     + " vy " + data.getYdot() * vkmpersec + " vz "
                     + data.getZdot() * vkmpersec);
            }
            System.out.println("time to process (millisec): "
                    + (System.currentTimeMillis()  - time1));
        } catch (ObjectDecayed ioe) {
            System.out.println("decayed " + ioe);
        } catch (SatElsetException ioe) {
            System.out.println("elset exception " + ioe);
        }

    }

    // Global variables
    private double ainv = 0.0;

    private double ao = 0.0;

    private double argpm = 0.0;

    private double cnodm = 0.0;

    private double con42 = 0.0;

    private double cosim = 0.0, sinim = 0.0, cosomm = 0.0, sinomm = 0.0;

    private double cosio = 0.0;

    private double cosio2 = 0.0;

    private double day = 0.0;

    private double dndt = 0.0;

    private double eccsq = 0.0;

    private double einv = 0.0;

    private double em = 0.0, emsq = 0.0, gam = 0.0, rtemsq = 0.0;

    private char help = 'n';

    private double inclm = 0.0;

    private double mm = 0.0;

    private double omegam = 0.0;

    private double omeosq = 0.0;

    private double pi = 3.14159265358979323846;

    private double twopi = 2.0 * pi;

    private double posq = 0.0;

    private double rad = 57.29577951308230;

    private double rp = 0.0;

    private double rteosq = 0.0;

    private double s1 = 0.0, s2 = 0.0, s3 = 0.0, s4 = 0.0, s5 = 0.0, s6 = 0.0;

    private double s7 = 0.0;

    private ElsetRec satrec = new ElsetRec();

    private double sinio = 0.0;

    private double snodm = 0.0;

    private double ss1 = 0.0, ss2 = 0.0, ss3 = 0.0;

    private double ss4 = 0.0, ss5 = 0.0, ss6 = 0.0, ss7 = 0.0;

    private double sz1 = 0.0;

    private double sz2 = 0.0, sz3 = 0.0, sz11 = 0.0, sz12 = 0.0, sz13 = 0.0;

    private double sz21 = 0.0, sz22 = 0.0, sz23 = 0.0, sz31 = 0.0, sz32 = 0.0;

    private double sz33 = 0.0, nm = 0.0;

    private TimeZone tz = GMT_TZ; // default to GMT

    private double z1 = 0.0;

    private double z2 = 0.0, z3 = 0.0, z11 = 0.0, z12 = 0.0, z13 = 0.0;

    private double z21 = 0.0, z22 = 0.0, z23 = 0.0, z31 = 0.0, z32 = 0.0;

    private double z33 = 0.0;

    /**
     * Default constructor
     * 
     */
    public Sgp4Unit() {

    }

    /**
     * Constructor with incoming SatElset
     * 
     * @param satElset SatElset
     */
    public Sgp4Unit(SatElset satElset) throws SatElsetException, ObjectDecayed {
        twoline2rv(satElset);
    }

    /*-----------------------------------------------------------------------------
     *
     *                           procedure dpper
     *
     *  this procedure provides deep space long period periodic contributions
     *    to the mean elements.  by design, these periodics are zero at epoch.
     *    this used to be dscom, but it's doublely a recurring function.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    e3          -
     *    ee2         -
     *    peo         -
     *    pgho        -
     *    pho         -
     *    pinco       -
     *    plo         -
     *    se2         -
     *    se3         -
     *    sgh2, sgh3, sgh4        -
     *    sh2, sh3
     *    si2, si3         -
     *    sl2, sl3, sl4         -
     *    t           -
     *    xgh2        -
     *    xgh3        -
     *    xgh4        -
     *    xh2, xh3    -
     *    xi2, xi3    -
     *    xl2, xl3, xl4         -
     *    zmol        -
     *    zmos        -
     *    ep          - eccentricity                           0.0 - 1.0
     *    inclp       - inclination
     *    omegap      - longitude of ascending node
     *    argpp       - argument of perigee
     *    mp          - mean anomaly
     *
     *  outputs       :
     *    ep          - eccentricity                           0.0 - 1.0
     *    inclp       - inclination
     *    omegap      - longitude of ascending node
     *    argpp       - argument of perigee
     *    mp          - mean anomaly
     *
     *  locals        :
     *    alfdp       -
     *    betdp       -
     *    cosip       -
     *    cosop       -
     *    dalf        -
     *    dbet        -
     *    dls         -
     *    f2          -
     *    f3          -
     *    pe          -
     *    pgh         -
     *    ph          -
     *    pinc        -
     *    pl          -
     *    sel         -
     *    ses         -
     *    sghl        -
     *    sghs        -
     *    shl         -
     *    shs         -
     *    sil         -
     *    sinip       -
     *    sinop       -
     *    sinzf       -
     *    sis         -
     *    sll         -
     *    sls         -
     *    xls         -
     *    xnoh        -
     *    zf          -
     *    zm          -
     *
     *  coupling      :
     *    none.
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    private void dpper(double e3, double ee2, double peo, double pgho,
            double pho, double pinco, double plo, double se2, double se3,
            double sgh2, double sgh3, double sgh4, double sh2, double sh3,
            double si2, double si3, double sl2, double sl3, double sl4,
            double t, double xgh2, double xgh3, double xgh4, double xh2,
            double xh3, double xi2, double xi3, double xl2, double xl3,
            double xl4, double zmol, double zmos, int init) {

        /* --------------------- local variables ------------------------ */
        double alfdp, betdp, cosip, cosop, dalf, dbet, dls, f2, f3;
        double pe, pgh, ph, pinc, pl, sel, ses, sghl, sghs, shll, shs;
        double sil, sinip, sinop, sinzf, sis, sll, sls, xls, xnoh, zf, zm;
        double zel, zes, znl, zns;

        /* ---------------------- constants ----------------------------- */
        zns = 1.19459e-5;
        zes = 0.01675;
        znl = 1.5835218e-4;
        zel = 0.05490;

        /* --------------- calculate time varying periodics ----------- */
        zm = zmos + zns * t;
        // be sure that the initial call has time set to zero (init will be 1)
        if (init != 0)
            zm = zmos;
        zf = zm + 2.0 * zes * Math.sin(zm);
        sinzf = Math.sin(zf);
        f2 = 0.5 * sinzf * sinzf - 0.25;
        f3 = -0.5 * sinzf * Math.cos(zf);
        ses = se2 * f2 + se3 * f3;
        sis = si2 * f2 + si3 * f3;
        sls = sl2 * f2 + sl3 * f3 + sl4 * sinzf;
        sghs = sgh2 * f2 + sgh3 * f3 + sgh4 * sinzf;
        shs = sh2 * f2 + sh3 * f3;
        zm = zmol + znl * t;
        if (init != 0)
            zm = zmol;
        zf = zm + 2.0 * zel * Math.sin(zm);
        sinzf = Math.sin(zf);
        f2 = 0.5 * sinzf * sinzf - 0.25;
        f3 = -0.5 * sinzf * Math.cos(zf);
        sel = ee2 * f2 + e3 * f3;
        sil = xi2 * f2 + xi3 * f3;
        sll = xl2 * f2 + xl3 * f3 + xl4 * sinzf;
        sghl = xgh2 * f2 + xgh3 * f3 + xgh4 * sinzf;
        shll = xh2 * f2 + xh3 * f3;
        pe = ses + sel;
        pinc = sis + sil;
        pl = sls + sll;
        pgh = sghs + sghl;
        ph = shs + shll;

        if (init == 0) {
            // set this up for lyddane fix per psc
            // these are routine calls (init = 0)
            // do before the inclination is changed
            // 0.2 rad = 11.45916 deg
            // sgp4fix for lyddane choice
            // af80 version checked original inclination
            // add next three lines to set up original inclination
            // ildm = 'y';
            // if (inclo >= 0.2)
            // ildm = 'n';

            pe = pe - peo;
            pinc = pinc - pinco;
            pl = pl - plo;
            pgh = pgh - pgho;
            ph = ph - pho;
            satrec.xincp = satrec.xincp + pinc;
            satrec.ep = satrec.ep + pe;
            sinip = Math.sin(satrec.xincp);
            cosip = Math.cos(satrec.xincp);

            /* ----------------- apply periodics directly ------------ */
            // could change this for the other side (pi - 0.2, but it affects
            // other tests
            // sgp4fix for lyddane choice
            // use this for original af80 approach and original inclination
            // we believe this is technically correct
            // if (ildm = 'n')
            // use this for perturbed inclination value per gsfc version
            // if (inclp >= 0.2)
            if (satrec.xincp >= 0.2) { // JMC
                // if (ilsd == 1) {
                ph = ph / sinip;
                pgh = pgh - cosip * ph;
                satrec.argpp = satrec.argpp + pgh;
                satrec.omegap = satrec.omegap + ph;
                satrec.mp = satrec.mp + pl;
            } else {
                /* ---- apply periodics with lyddane modification ---- */
                sinop = Math.sin(satrec.omegap);
                cosop = Math.cos(satrec.omegap);
                alfdp = sinip * sinop;
                betdp = sinip * cosop;
                dalf = ph * cosop + pinc * cosip * sinop;
                dbet = -ph * sinop + pinc * cosip * cosop;
                alfdp = alfdp + dalf;
                betdp = betdp + dbet;
                satrec.omegap = modfunc(satrec.omegap, twopi);
                xls = satrec.mp + satrec.argpp + cosip * satrec.omegap;
                dls = pl + pgh - pinc * satrec.omegap * sinip;
                xls = xls + dls;
                xnoh = satrec.omegap;
                satrec.omegap = Math.atan2(alfdp, betdp);
                if (Math.abs(xnoh - satrec.omegap) > pi)
                    if (Math.abs(xnoh - satrec.omegap) > pi)
                        if (satrec.omegap < xnoh)
                            satrec.omegap = satrec.omegap + twopi;
                        else
                            satrec.omegap = satrec.omegap - twopi;
                satrec.mp = satrec.mp + pl;
                satrec.argpp = xls - satrec.mp - cosip * satrec.omegap;
            }

        }

        if (help == 'y')
        // if (dbgfile != NULL)
        {
            System.out.printf("%84s\n",
                    " ------------------after dpper : ---------------- ");
            System.out.printf("    inputs : \n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "e3", e3, "ee2", ee2, "peo", peo, "pgho", pgho, "pho", pho,
                    "pinco", pinco);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "plo", plo, "se2", se2, "se3", se3, "sgh2", sgh2, "sgh3",
                    sgh3, "sgh4", sgh4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sh2", sh2, "sh3", sh3, "si2", si2, "si3", si3, "sl2", sl2,
                    "sl3", sl3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sl4", sl4, "t", t, "xgh2", xgh2, "xgh3", xgh3, "xgh4",
                    xgh4, "xh2", xh2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xh3", xh3, "xi2", xi2, "xi3", xi3, "xl2", xl2, "xl3", xl3,
                    "xl4", xl4);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15d\n", "zmol", zmol,
                    "zmos", zmos, "init", init);
            System.out.printf("    in/out : \n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "ep",
                    satrec.ep, "inclp", satrec.xincp, "omegap", satrec.omegap,
                    "argpp", satrec.argpp, "mp", satrec.mp);
        }
    }

    /*-----------------------------------------------------------------------------
     *
     *                           procedure dscom
     *
     *  this procedure provides deep space common items used by both the secular
     *    and periodics subroutines.  input is provided as shown. this routine
     *    used to be called dpper, but the functions inside weren't correct.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    epoch       -
     *    ep          - eccentricity
     *    argpp       - argument of perigee
     *    tc          -
     *    inclp       - inclination
     *    omegap      - longitude of ascending node
     *    np          - mean motion
     *
     *  outputs       :
     *    sinim       -
     *    cosim       -
     *    sinomm      -
     *    cosomm      -
     *    snodm       -
     *    cnodm       -
     *    day         -
     *    e3          -
     *    ee2         -
     *    em          - eccentricity
     *    emsq        - eccentricity squared
     *    gam         -
     *    peo         -
     *    pgho        -
     *    pho         -
     *    pinco       -
     *    plo         -
     *    rtemsq      -
     *    se2, se3         -
     *    sgh2, sgh3, sgh4        -
     *    sh2, sh3
     *    si2, si3         -
     *    sl2, sl3, sl4         -
     *    s1, s2, s3, s4, s5, s6, s7          -
     *    ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
     *    sz1, sz2, sz3         -
     *    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
     *    xgh2, xgh3, xgh4        -
     *    xh2, xh3         -
     *    xi2, xi3         -
     *    xl2, xl3, xl4         -
     *    nm          - mean motion
     *    z1, z2, z3          -
     *    z11, z12, z13, z21, z22, z23, z31, z32, z33         -
     *    zmol        -
     *    zmos        -
     *
     *  locals        :
     *    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10         -
     *    betasq      -
     *    cc          -
     *    ctem        -
     *    stem        -
     *    x1, x2, x3, x4, x5, x6, x7, x8          -
     *    xnodce      -
     *    xnoi        -
     *    zcosg       -
     *    zcosgl      -
     *    zcosh       -
     *    zcoshl      -
     *    zcosi       -
     *    zcosil      -
     *    zsing       -
     *    zsingl      -
     *    zsinh       -
     *    zsinhl      -
     *    zsini       -
     *    zsinil      -
     *    zx          -
     *    zy          -
     *
     *  coupling      :
     *    none.
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    private void dscom(double epoch, double ep, double argpp, double tc,
            double inclp, double omegap, double np) {

        /* -------------------------- constants ------------------------- */
        double zes = 0.01675;
        double zel = 0.05490;
        double c1ss = 2.9864797e-6;
        double c1l = 4.7968065e-7;
        double zsinis = 0.39785416;
        double zcosis = 0.91744867;
        double zcosgs = 0.1945905;
        double zsings = -0.98088458;

        /* --------------------- local variables ------------------------ */
        int lsflg;
        double a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, betasq, cc, ctem, stem, x1, x2, x3, x4, x5, x6, x7, x8, xnodce, xnoi, zcosg, zcosgl, zcosh, zcoshl, zcosi, zcosil, zsing, zsingl, zsinh, zsinhl, zsini, zsinil, zx, zy;

        nm = np;
        em = ep;
        snodm = Math.sin(omegap);
        cnodm = Math.cos(omegap);
        sinomm = Math.sin(argpp);
        cosomm = Math.cos(argpp);
        sinim = Math.sin(inclp);
        cosim = Math.cos(inclp);
        emsq = em * em;
        betasq = 1.0 - emsq;
        rtemsq = Math.sqrt(betasq);

        /* ----------------- initialize lunar solar terms --------------- */
        satrec.dsvalues.peo = 0.0;
        satrec.dsvalues.pinco = 0.0;
        satrec.dsvalues.plo = 0.0;
        satrec.dsvalues.pgho = 0.0;
        satrec.dsvalues.pho = 0.0;
        day = epoch + 18261.5 + tc / 1440.0;
        xnodce = modfunc(4.5236020 - 9.2422029e-4 * day, twopi);
        stem = Math.sin(xnodce);
        ctem = Math.cos(xnodce);
        zcosil = 0.91375164 - 0.03568096 * ctem;
        zsinil = Math.sqrt(1.0 - zcosil * zcosil);
        zsinhl = 0.089683511 * stem / zsinil;
        zcoshl = Math.sqrt(1.0 - zsinhl * zsinhl);
        gam = 5.8351514 + 0.0019443680 * day;
        zx = 0.39785416 * stem / zsinil;
        zy = zcoshl * ctem + 0.91744867 * zsinhl * stem;
        zx = Math.atan2(zx, zy);
        zx = gam + zx - xnodce;
        zcosgl = Math.cos(zx);
        zsingl = Math.sin(zx);

        /* ------------------------- do solar terms --------------------- */
        zcosg = zcosgs;
        zsing = zsings;
        zcosi = zcosis;
        zsini = zsinis;
        zcosh = cnodm;
        zsinh = snodm;
        cc = c1ss;
        xnoi = 1.0 / nm;

        for (lsflg = 1; lsflg <= 2; lsflg++) {
            a1 = zcosg * zcosh + zsing * zcosi * zsinh;
            a3 = -zsing * zcosh + zcosg * zcosi * zsinh;
            a7 = -zcosg * zsinh + zsing * zcosi * zcosh;
            a8 = zsing * zsini;
            a9 = zsing * zsinh + zcosg * zcosi * zcosh;
            a10 = zcosg * zsini;
            a2 = cosim * a7 + sinim * a8;
            a4 = cosim * a9 + sinim * a10;
            a5 = -sinim * a7 + cosim * a8;
            a6 = -sinim * a9 + cosim * a10;

            x1 = a1 * cosomm + a2 * sinomm;
            x2 = a3 * cosomm + a4 * sinomm;
            x3 = -a1 * sinomm + a2 * cosomm;
            x4 = -a3 * sinomm + a4 * cosomm;
            x5 = a5 * sinomm;
            x6 = a6 * sinomm;
            x7 = a5 * cosomm;
            x8 = a6 * cosomm;

            z31 = 12.0 * x1 * x1 - 3.0 * x3 * x3;
            z32 = 24.0 * x1 * x2 - 6.0 * x3 * x4;
            z33 = 12.0 * x2 * x2 - 3.0 * x4 * x4;
            z1 = 3.0 * (a1 * a1 + a2 * a2) + z31 * emsq;
            z2 = 6.0 * (a1 * a3 + a2 * a4) + z32 * emsq;
            z3 = 3.0 * (a3 * a3 + a4 * a4) + z33 * emsq;
            z11 = -6.0 * a1 * a5 + emsq * (-24.0 * x1 * x7 - 6.0 * x3 * x5);
            z12 = -6.0 * (a1 * a6 + a3 * a5) + emsq
                    * (-24.0 * (x2 * x7 + x1 * x8) - 6.0 * (x3 * x6 + x4 * x5));
            z13 = -6.0 * a3 * a6 + emsq * (-24.0 * x2 * x8 - 6.0 * x4 * x6);
            z21 = 6.0 * a2 * a5 + emsq * (24.0 * x1 * x5 - 6.0 * x3 * x7);
            z22 = 6.0 * (a4 * a5 + a2 * a6) + emsq
                    * (24.0 * (x2 * x5 + x1 * x6) - 6.0 * (x4 * x7 + x3 * x8));
            z23 = 6.0 * a4 * a6 + emsq * (24.0 * x2 * x6 - 6.0 * x4 * x8);
            z1 = z1 + z1 + betasq * z31;
            z2 = z2 + z2 + betasq * z32;
            z3 = z3 + z3 + betasq * z33;
            s3 = cc * xnoi;
            s2 = -0.5 * s3 / rtemsq;
            s4 = s3 * rtemsq;
            s1 = -15.0 * em * s4;
            s5 = x1 * x3 + x2 * x4;
            s6 = x2 * x3 + x1 * x4;
            s7 = x2 * x4 - x1 * x3;

            /* ----------------------- do lunar terms ------------------- */
            if (lsflg == 1) {
                ss1 = s1;
                ss2 = s2;
                ss3 = s3;
                ss4 = s4;
                ss5 = s5;
                ss6 = s6;
                ss7 = s7;
                sz1 = z1;
                sz2 = z2;
                sz3 = z3;
                sz11 = z11;
                sz12 = z12;
                sz13 = z13;
                sz21 = z21;
                sz22 = z22;
                sz23 = z23;
                sz31 = z31;
                sz32 = z32;
                sz33 = z33;
                zcosg = zcosgl;
                zsing = zsingl;
                zcosi = zcosil;
                zsini = zsinil;
                zcosh = zcoshl * cnodm + zsinhl * snodm;
                zsinh = snodm * zcoshl - cnodm * zsinhl;
                cc = c1l;
            }
        }

        satrec.dsvalues.zmol = modfunc(4.7199672 + 0.22997150 * day - gam,
                twopi);
        satrec.dsvalues.zmos = modfunc(6.2565837 + 0.017201977 * day, twopi);

        /* ------------------------ do solar terms ---------------------- */
        satrec.dsvalues.se2 = 2.0 * ss1 * ss6;
        satrec.dsvalues.se3 = 2.0 * ss1 * ss7;
        satrec.dsvalues.si2 = 2.0 * ss2 * sz12;
        satrec.dsvalues.si3 = 2.0 * ss2 * (sz13 - sz11);
        satrec.dsvalues.sl2 = -2.0 * ss3 * sz2;
        satrec.dsvalues.sl3 = -2.0 * ss3 * (sz3 - sz1);
        satrec.dsvalues.sl4 = -2.0 * ss3 * (-21.0 - 9.0 * emsq) * zes;
        satrec.dsvalues.sgh2 = 2.0 * ss4 * sz32;
        satrec.dsvalues.sgh3 = 2.0 * ss4 * (sz33 - sz31);
        satrec.dsvalues.sgh4 = -18.0 * ss4 * zes;
        satrec.dsvalues.sh2 = -2.0 * ss2 * sz22;
        satrec.dsvalues.sh3 = -2.0 * ss2 * (sz23 - sz21);

        /* ------------------------ do lunar terms ---------------------- */
        satrec.dsvalues.ee2 = 2.0 * s1 * s6;
        satrec.dsvalues.e3 = 2.0 * s1 * s7;
        satrec.dsvalues.xi2 = 2.0 * s2 * z12;
        satrec.dsvalues.xi3 = 2.0 * s2 * (z13 - z11);
        satrec.dsvalues.xl2 = -2.0 * s3 * z2;
        satrec.dsvalues.xl3 = -2.0 * s3 * (z3 - z1);
        satrec.dsvalues.xl4 = -2.0 * s3 * (-21.0 - 9.0 * emsq) * zel;
        satrec.dsvalues.xgh2 = 2.0 * s4 * z32;
        satrec.dsvalues.xgh3 = 2.0 * s4 * (z33 - z31);
        satrec.dsvalues.xgh4 = -18.0 * s4 * zel;
        satrec.dsvalues.xh2 = -2.0 * s2 * z22;
        satrec.dsvalues.xh3 = -2.0 * s2 * (z23 - z21);

        if (help == 'y')
        // if (dbgfile != NULL)
        {
            System.out.printf("%84s\n",
                    " ------------------ after dscom :-----------------");
            System.out.printf("    inputs :\n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "epoch ", epoch, "ep", ep, "argpp", argpp, "tc", tc,
                    "inclp", inclp, "omegap", omegap);
            System.out.printf("%7s%15.9f\n", "np", np);
            System.out.printf("    outputs :\n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "snodm", snodm, "cnodm", cnodm, "sinim", sinim, "cosim",
                    cosim, "sinomm", sinomm, "cosomm", cosomm);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "day", day, "e3", satrec.dsvalues.e3, "ee2",
                    satrec.dsvalues.ee2, "em", em, "emsq", emsq, "gam", gam);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "peo",
                    satrec.dsvalues.peo, "pgho", satrec.dsvalues.pgho, "pho",
                    satrec.dsvalues.pho, "pinco", satrec.dsvalues.pinco, "plo",
                    satrec.dsvalues.plo);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "rtemsq", rtemsq, "se2", satrec.dsvalues.se2, "se3",
                    satrec.dsvalues.se3, "sgh2", satrec.dsvalues.sgh2, "sgh3",
                    satrec.dsvalues.sgh3, "sgh4", satrec.dsvalues.sgh4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sh2", satrec.dsvalues.sh2, "sh3", satrec.dsvalues.sh3,
                    "si2", satrec.dsvalues.si2, "si3", satrec.dsvalues.si3,
                    "sl2", satrec.dsvalues.sl2, "sl3", satrec.dsvalues.sl3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sl4", satrec.dsvalues.sl4, "s1", s1, "s2", s2, "s3", s3,
                    "s4", s4, "s5", s5);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "s6", s6, "s7", s7, "ss1", ss1, "ss2", ss2, "ss3", ss3,
                    "ss4", ss4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "ss5", ss5, "ss6", ss6, "ss7", ss7, "sz1", sz1, "sz2", sz2,
                    "sz3", sz3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sz11", sz11, "sz12", sz12, "sz13", sz13, "sz21", sz21,
                    "sz22", sz22, "sz23", sz23);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sz31", sz31, "sz32", sz32, "sz33", sz33, "xgh2",
                    satrec.dsvalues.xgh2, "xgh3", satrec.dsvalues.xgh3, "xgh4",
                    satrec.dsvalues.xgh4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xh2", satrec.dsvalues.xh2, "xh3", satrec.dsvalues.xh3,
                    "xi2", satrec.dsvalues.xi2, "xi3", satrec.dsvalues.xi3,
                    "xl2", satrec.dsvalues.xl2, "xl3", satrec.dsvalues.xl3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xl4", satrec.dsvalues.xl4, "nm", nm, "z1", z1, "z2", z2,
                    "z3", z3, "z11", z11);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "z12", z12, "z13", z13, "z21", z21, "z22", z22, "z23", z23,
                    "z31", z31);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "z32",
                    z32, "z33", z33, "zmol", satrec.dsvalues.zmol, "zmos",
                    satrec.dsvalues.zmos);
        }
    }

    /*-----------------------------------------------------------------------------
     *
     *                           procedure dsinit
     *
     *  this procedure provides deep space contributions to mean motion dot due
     *    to geopotential resonance with half day and one day orbits.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    cosim       -
     *    emsq        - eccentricity squared
     *    argpo       - argument of perigee
     *    s1, s2, s3, s4, s5          -
     *    sinim       -
     *    ss1, ss2, ss3, ss4, ss5         -
     *    sz1, sz3
     *    sz11, sz13, sz21, sz23, sz31, sz33        -
     *    t           - time
     *    tc          -
     *    gsto        -
     *    mo          - mean anomaly
     *    mdot        - mean anomaly dot (rate)
     *    no          - mean motion
     *    omegao      - longitude of ascending node
     *    omegadot    - longitude of ascending node dot (rate)
     *    xpidot      -
     *    z1, z3      -
     *    z11, z13, z21, z23, z31, z33         -
     *    em          - eccentricity
     *    argpm       - argument of perigee
     *    inclm       - inclination
     *    mm          - mean anomaly
     *    nm          - mean motion
     *    omegam      - longitude of ascending node
     *
     *  outputs       :
     *    em          - eccentricity
     *    argpm       - argument of perigee
     *    inclm       - inclination
     *    mm          - mean anomaly
     *    nm          - mean motion
     *    omegam      - longitude of ascending node
     *    irez        - flag for resonances               1-one day      - 2-half day
     *    atime       -
     *    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
     *    dedt        -
     *    didt        -
     *    dmdt        -
     *    dndt        -
     *    dnodt       -
     *    domdt       -
     *    del1, del2, del3        -
     *    ses         -
     *    sghl        -
     *    sghs        -
     *    sgs         -
     *    shl         -
     *    shs         -
     *    sis         -
     *    sls         -
     *    theta       -
     *    xfact       -
     *    xlamo       -
     *    xli         -
     *    xni
     *
     *  locals        :
     *    ainv2       -
     *    aonv        -
     *    cosisq      -
     *    eoc         -
     *    f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543        -
     *    g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533        -
     *    sini2       -
     *    temp        -
     *    temp1       -
     *    theta       -
     *    xno2        -
     *
     *  coupling      :
     *    none.
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    private void dsinit(double cosim, double emsq, double argpo, double s1,
            double s2, double s3, double s4, double s5, double sinim,
             double t, double tc, double gsto, double mo, double mdot,
            double no, double omegao, double omegadot, double xpidot,
            double z1, double z3, double z11, double z13, double z21,
            double z23, double z31, double z33, double ecco, double eccsq) {

        /* --------------------- local variables ------------------------ */

        double ainv2, aonv = 0.0, cosisq, eoc, f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543, g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533, ses, sgs, sghl, sghs, shs, shll, sis, sini2, sls, temp, temp1, theta, xno2, q22, q31, q33, root22, root44, root54, rptim, root32, root52, x2o3, xke, znl, emo, zns, emsqo;

        q22 = 1.7891679e-6;
        q31 = 2.1460748e-6;
        q33 = 2.2123015e-7;
        root22 = 1.7891679e-6;
        root44 = 7.3636953e-9;
        root54 = 2.1765803e-9;
        rptim = 4.37526908801129966e-3;
        root32 = 3.7393792e-7;
        root52 = 1.1428639e-7;
        x2o3 = 2.0 / 3.0;
        xke = 7.43669161331734132e-2;
        znl = 1.5835218e-4;
        zns = 1.19459e-5;

        /* -------------------- deep space initialization ------------ */
        satrec.dsvalues.irez = 0;
        if ((nm < 0.0052359877) && (nm > 0.0034906585))
            satrec.dsvalues.irez = 1;
        if ((nm >= 8.26e-3) && (nm <= 9.24e-3) && (em >= 0.5))
            satrec.dsvalues.irez = 2;

        /* ------------------------ do solar terms ------------------- */
        ses = ss1 * zns * ss5;
        sis = ss2 * zns * (sz11 + sz13);
        sls = -zns * ss3 * (sz1 + sz3 - 14.0 - 6.0 * emsq);
        sghs = ss4 * zns * (sz31 + sz33 - 6.0);
        shs = -zns * ss2 * (sz21 + sz23);
        if (inclm < 5.2359877e-2)
            shs = 0.0;
        if (sinim != 0.0)
            shs = shs / sinim;
        sgs = sghs - cosim * shs;

        /* ------------------------- do lunar terms ------------------ */
        satrec.dsvalues.dedt = ses + s1 * znl * s5;
        satrec.dsvalues.didt = sis + s2 * znl * (z11 + z13);
        satrec.dsvalues.dmdt = sls - znl * s3 * (z1 + z3 - 14.0 - 6.0 * emsq);
        sghl = s4 * znl * (z31 + z33 - 6.0);
        shll = -znl * s2 * (z21 + z23);
        if (inclm < 5.2359877e-2)
            shll = 0.0;
        satrec.dsvalues.domdt = sgs + sghl;
        satrec.dsvalues.dnodt = shs;
        if (sinim != 0.0) {
            satrec.dsvalues.domdt = satrec.dsvalues.domdt - cosim / sinim
                    * shll;
            satrec.dsvalues.dnodt = satrec.dsvalues.dnodt + shll / sinim;
        }

        /* ----------- calculate deep space resonance effects -------- */
        dndt = 0.0;
        theta = modfunc(gsto + tc * rptim, twopi);
        em = em + satrec.dsvalues.dedt * t;
        // shouldn't emsq be changed now?????
        inclm = inclm + satrec.dsvalues.didt * t;
        argpm = argpm + satrec.dsvalues.domdt * t;
        omegam = omegam + satrec.dsvalues.dnodt * t;
        mm = mm + satrec.dsvalues.dmdt * t;
        /*
         * these are not needed since they do not get into any DS structs, and
         * are not used if (inclm < 0.0) { inclm = -inclm; argpm = argpm - pi;
         * omegam = omegam + pi; }
         */
        // sgp4fix for negative inclinations
        // the following if statement should be commented out
        // if (inclm < 0.0)
        // {
        // inclm = -inclm;
        // argpm = argpm - pi;
        // omegam = omegam + pi;
        // }
        /* -------------- initialize the resonance terms ------------- */
        if (satrec.dsvalues.irez != 0)
            aonv = Math.pow(nm / xke, x2o3);

        /* ---------- geopotential resonance for 12 hour orbits ------ */
        if (satrec.dsvalues.irez == 2) {
            cosisq = cosim * cosim;
            emo = em;
            em = ecco;
            emsqo = emsq;
            emsq = eccsq;
            eoc = em * emsq;
            g201 = -0.306 - (em - 0.64) * 0.440;

            if (em <= 0.65) {
                g211 = 3.616 - 13.2470 * em + 16.2900 * emsq;
                g310 = -19.302 + 117.3900 * em - 228.4190 * emsq + 156.5910
                        * eoc;
                g322 = -18.9068 + 109.7927 * em - 214.6334 * emsq + 146.5816
                        * eoc;
                g410 = -41.122 + 242.6940 * em - 471.0940 * emsq + 313.9530
                        * eoc;
                g422 = -146.407 + 841.8800 * em - 1629.014 * emsq + 1083.4350
                        * eoc;
                g520 = -532.114 + 3017.977 * em - 5740.032 * emsq + 3708.2760
                        * eoc;
            } else {
                g211 = -72.099 + 331.819 * em - 508.738 * emsq + 266.724 * eoc;
                g310 = -346.844 + 1582.851 * em - 2415.925 * emsq + 1246.113
                        * eoc;
                g322 = -342.585 + 1554.908 * em - 2366.899 * emsq + 1215.972
                        * eoc;
                g410 = -1052.797 + 4758.686 * em - 7193.992 * emsq + 3651.957
                        * eoc;
                g422 = -3581.690 + 16178.110 * em - 24462.770 * emsq
                        + 12422.520 * eoc;
                if (em > 0.715)
                    g520 = -5149.66 + 29936.92 * em - 54087.36 * emsq
                            + 31324.56 * eoc;
                else
                    g520 = 1464.74 - 4664.75 * em + 3763.64 * emsq;
            }
            if (em < 0.7) {
                g533 = -919.22770 + 4988.6100 * em - 9064.7700 * emsq + 5542.21
                        * eoc;
                g521 = -822.71072 + 4568.6173 * em - 8491.4146 * emsq
                        + 5337.524 * eoc;
                g532 = -853.66600 + 4690.2500 * em - 8624.7700 * emsq + 5341.4
                        * eoc;
            } else {
                g533 = -37995.780 + 161616.52 * em - 229838.20 * emsq
                        + 109377.94 * eoc;
                g521 = -51752.104 + 218913.95 * em - 309468.16 * emsq
                        + 146349.42 * eoc;
                g532 = -40023.880 + 170470.89 * em - 242699.48 * emsq
                        + 115605.82 * eoc;
            }

            sini2 = sinim * sinim;
            f220 = 0.75 * (1.0 + 2.0 * cosim + cosisq);
            f221 = 1.5 * sini2;
            f321 = 1.875 * sinim * (1.0 - 2.0 * cosim - 3.0 * cosisq);
            f322 = -1.875 * sinim * (1.0 + 2.0 * cosim - 3.0 * cosisq);
            f441 = 35.0 * sini2 * f220;
            f442 = 39.3750 * sini2 * sini2;
            f522 = 9.84375
                    * sinim
                    * (sini2 * (1.0 - 2.0 * cosim - 5.0 * cosisq) + 0.33333333 * (-2.0
                            + 4.0 * cosim + 6.0 * cosisq));
            f523 = sinim
                    * (4.92187512 * sini2
                            * (-2.0 - 4.0 * cosim + 10.0 * cosisq) + 6.56250012 * (1.0 + 2.0 * cosim - 3.0 * cosisq));
            f542 = 29.53125
                    * sinim
                    * (2.0 - 8.0 * cosim + cosisq
                            * (-12.0 + 8.0 * cosim + 10.0 * cosisq));
            f543 = 29.53125
                    * sinim
                    * (-2.0 - 8.0 * cosim + cosisq
                            * (12.0 + 8.0 * cosim - 10.0 * cosisq));
            xno2 = nm * nm;
            ainv2 = aonv * aonv;
            temp1 = 3.0 * xno2 * ainv2;
            temp = temp1 * root22;
            satrec.dsvalues.d2201 = temp * f220 * g201;
            satrec.dsvalues.d2211 = temp * f221 * g211;
            temp1 = temp1 * aonv;
            temp = temp1 * root32;
            satrec.dsvalues.d3210 = temp * f321 * g310;
            satrec.dsvalues.d3222 = temp * f322 * g322;
            temp1 = temp1 * aonv;
            temp = 2.0 * temp1 * root44;
            satrec.dsvalues.d4410 = temp * f441 * g410;
            satrec.dsvalues.d4422 = temp * f442 * g422;
            temp1 = temp1 * aonv;
            temp = temp1 * root52;
            satrec.dsvalues.d5220 = temp * f522 * g520;
            satrec.dsvalues.d5232 = temp * f523 * g532;
            temp = 2.0 * temp1 * root54;
            satrec.dsvalues.d5421 = temp * f542 * g521;
            satrec.dsvalues.d5433 = temp * f543 * g533;
            satrec.dsvalues.xlamo = modfunc(mo + omegao + omegao - theta
                    - theta, twopi);
            satrec.dsvalues.xfact = mdot + satrec.dsvalues.dmdt + 2.0
                    * (omegadot + satrec.dsvalues.dnodt - rptim) - no;
            em = emo;
            emsq = emsqo;
        }

        /* ---------------- synchronous resonance terms -------------- */
        if (satrec.dsvalues.irez == 1) {
            g200 = 1.0 + emsq * (-2.5 + 0.8125 * emsq);
            g310 = 1.0 + 2.0 * emsq;
            g300 = 1.0 + emsq * (-6.0 + 6.60937 * emsq);
            f220 = 0.75 * (1.0 + cosim) * (1.0 + cosim);
            f311 = 0.9375 * sinim * sinim * (1.0 + 3.0 * cosim) - 0.75
                    * (1.0 + cosim);
            f330 = 1.0 + cosim;
            f330 = 1.875 * f330 * f330 * f330;
            satrec.dsvalues.del1 = 3.0 * nm * nm * aonv * aonv;
            satrec.dsvalues.del2 = 2.0 * satrec.dsvalues.del1 * f220 * g200
                    * q22;
            satrec.dsvalues.del3 = 3.0 * satrec.dsvalues.del1 * f330 * g300
                    * q33 * aonv;
            satrec.dsvalues.del1 = satrec.dsvalues.del1 * f311 * g310 * q31
                    * aonv;
            satrec.dsvalues.xlamo = modfunc(mo + omegao + argpo - theta, twopi);
            satrec.dsvalues.xfact = mdot + xpidot - rptim
                    + satrec.dsvalues.dmdt + satrec.dsvalues.domdt
                    + satrec.dsvalues.dnodt - no;
        }

        /* ------------ for sgp4, initialize the integrator ---------- */
        if (satrec.dsvalues.irez != 0) {
            satrec.dsvalues.xli = satrec.dsvalues.xlamo;
            satrec.dsvalues.xni = no;
            satrec.dsvalues.atime = 0.0;
            nm = no + dndt;
        }

        if (help == 'y')
        // if (dbgfile != NULL)
        {

            System.out.printf("%84s\n",
                    " ------------------after dsinit---------------");
            System.out.printf(" inputs :\n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "cosim", cosim, "emsq", emsq, "argpo", argpo, "s1", s1,
                    "s2", s2, "s3", s3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "s4", s4, "s5", s5, "sinim", sinim, "ss1", ss1, "ss2", ss2,
                    "ss3", ss3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "ss4", ss4, "ss5", ss5, "sz1", sz1, "sz3", sz3, "sz11",
                    sz11, "sz13", sz13);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sz21", sz21, "sz23", sz23, "sz31", sz31, "sz33", sz33,
                    "t", t, "tc", tc);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "gsto", gsto, "mo", mo, "mdot", mdot, "no", no, "omegao",
                    omegao, "omegadt", omegadot);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xpidot", xpidot, "z1", z1, "z3", z3, "z11", z11, "z13",
                    z13, "z21", z21);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "z23", z23,
                    "z31", z31, "z33", z33);
            System.out.printf(" in /out :\n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "em", em, "argpm", argpm, "inclm", inclm, "mm", mm, "nm",
                    nm, "omegam", omegam);
            System.out.printf(" outputs :\n");
            System.out.printf(
                    "%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "irez", satrec.dsvalues.irez, "atime",
                    satrec.dsvalues.atime, "d2201", satrec.dsvalues.d2201,
                    "d2211", satrec.dsvalues.d2211, "d3210",
                    satrec.dsvalues.d3210, "d3222", satrec.dsvalues.d3222);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "d4410", satrec.dsvalues.d4410, "d4422",
                    satrec.dsvalues.d4422, "d5220", satrec.dsvalues.d5220,
                    "d5232", satrec.dsvalues.d5232, "d5421",
                    satrec.dsvalues.d5421, "d5433", satrec.dsvalues.d5433);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "dedt", satrec.dsvalues.dedt, "didt", satrec.dsvalues.didt,
                    "dmdt", satrec.dsvalues.dmdt, "dndt", dndt, "dnodt",
                    satrec.dsvalues.dnodt, "domdt", satrec.dsvalues.domdt);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "del1", satrec.dsvalues.del1, "del2", satrec.dsvalues.del2,
                    "del3", satrec.dsvalues.del3, "xfact",
                    satrec.dsvalues.xfact, "xlamo", satrec.dsvalues.xlamo,
                    "xli", satrec.dsvalues.xli);
            System.out.printf("%7s%15.9f\n", "xni", satrec.dsvalues.xni);

        }
    }

    /*-----------------------------------------------------------------------------
     *
     *                           procedure dspace
     *
     *  this procedure provides deep space contributions to mean elements for
     *    perturbing third body.  these effects ave been averaged over one
     *    revolution of the sun and moon.  for earth resonance effects, the
     *    effects have been averaged over no revolutions of the satellite. (mean motion)
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
     *    dedt        -
     *    del1        -
     *    del2        -
     *    del3        -
     *    didt        -
     *    dmdt        -
     *    dnodt       -
     *    domdt       -
     *    irez        -
     *    argpo       - argument of perigee
     *    argpdot     - argument of perigee dot (rate)
     *    t           - time
     *    tc          -
     *    gsto        - gst
     *    xfact       -
     *    xlamo       -
     *    no          - mean motion
     *    atime       -
     *    em          - eccentricity
     *    ft          -
     *    argpm       - argument of perigee
     *    inclm       - inclination
     *    xli         -
     *    mm          - mean anomaly
     *    xni         - mean motion
     *    omegam      - longitude of ascending node
     *
     *  outputs       :
     *    atime       -
     *    em          - eccentricity
     *    argpm       - argument of perigee
     *    inclm       - inclination
     *    xli         -
     *    mm          - mean anomaly
     *    xni         -
     *    omegam      - longitude of ascending node
     *    dndt        -
     *    nm          - mean motion
     *
     *  locals        :
     *    delt        -
     *    ft          -
     *    theta       -
     *    x2li        -
     *    x2omi       -
     *    xl          -
     *    xldot       -
     *    xnddt       -
     *    xndt        -
     *    xomi        -
     *
     *  coupling      :
     *    srez        -
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    private void dspace(int irez, double d2201, double d2211, double d3210,
            double d3222, double d4410, double d4422, double d5220,
            double d5232, double d5421, double d5433, double dedt, double del1,
            double del2, double del3, double didt, double dmdt, double dnodt,
            double domdt, double argpo, double argpdot, double t, double tc,
            double gsto, double xfact, double xlamo, double no)
    {

        int iretn, iret;
        double delt, ft = 0.0, theta, x2li, x2omi, xl, xldot = 0.0, xnddt = 0.0, xndt = 0.0, xomi, g22, g32, g44, g52, g54, fasx2, fasx4, fasx6, rptim, step2, stepn, stepp;

        fasx2 = 0.13130908;
        fasx4 = 2.8843198;
        fasx6 = 0.37448087;
        g22 = 5.7686396;
        g32 = 0.95240898;
        g44 = 1.8014998;
        g52 = 1.0508330;
        g54 = 4.4108898;
        rptim = 4.37526908801129966e-3;
        stepp = 720.0;
        stepn = -720.0;
        step2 = 259200.0;

        /* ----------- calculate deep space resonance effects ----------- */
        dndt = 0.0;
        theta = modfunc(gsto + tc * rptim, twopi);
        em = em + dedt * t;
        // shouldn't emsq be changed now?????
        inclm = inclm + didt * t;
        argpm = argpm + domdt * t;
        omegam = omegam + dnodt * t;
        mm = mm + dmdt * t;
        // sgp4fix for negative inclinations
        // the following if statement should be commented out
        // if (inclm < 0.0)
        // {
        // inclm = -inclm;
        // argpm = argpm - pi;
        // omegam = omegam + pi;
        // }

        /* - update resonances : numerical (euler-maclaurin) integration - */
        /* ------------------------- epoch restart ---------------------- */
        ft = 0.0;
        satrec.dsvalues.atime = 0.0;
        if (irez != 0) {
            if ((satrec.dsvalues.atime == 0.0)
                    || ((t >= 0.0) && (satrec.dsvalues.atime < 0.0))
                    || ((t < 0.0) && (satrec.dsvalues.atime >= 0.0))) {
                if (t >= 0.0)
                    delt = stepp;
                else
                    delt = stepn;
                satrec.dsvalues.atime = 0.0;
                satrec.dsvalues.xni = no;
                satrec.dsvalues.xli = xlamo;
            }
            iretn = 381; // added for do loop
            iret = 0; // added for loop
            while (iretn == 381) {
                // if ((fabs(t) < fabs(atime)) || (iret == 351))
                if ((Math.abs(t) < Math.abs(satrec.dsvalues.atime))
                        || (iret == 351)) {
                    if (t >= 0.0)
                        delt = stepn;
                    else
                        delt = stepp;
                    iret = 351;
                    iretn = 381;
                } else {
                    if (t > 0.0) // error if prev if has atime:=0.0 and
                        // t:=0.0 (ge)
                        delt = stepp;
                    else
                        delt = stepn;
                    // if (fabs(t - atime) >= stepp)
                    if (Math.abs(t - satrec.dsvalues.atime) >= stepp) {
                        iret = 0;
                        iretn = 381;
                    } else {
                        ft = t - satrec.dsvalues.atime;
                        iretn = 0;
                    }
                }

                /* ------------------- dot terms calculated ------------- */
                /* ----------- near - synchronous resonance terms ------- */
                if (irez != 2) {
                    xndt = del1 * Math.sin(satrec.dsvalues.xli - fasx2) + del2
                            * Math.sin(2.0 * (satrec.dsvalues.xli - fasx4))
                            + del3
                            * Math.sin(3.0 * (satrec.dsvalues.xli - fasx6));
                    xldot = satrec.dsvalues.xni + xfact;
                    xnddt = del1 * Math.cos(satrec.dsvalues.xli - fasx2) + 2.0
                            * del2
                            * Math.cos(2.0 * (satrec.dsvalues.xli - fasx4))
                            + 3.0 * del3
                            * Math.cos(3.0 * (satrec.dsvalues.xli - fasx6));
                    xnddt = xnddt * xldot;
                } else {
                    /* --------- near - half-day resonance terms -------- */
                    xomi = argpo + argpdot * satrec.dsvalues.atime;
                    x2omi = xomi + xomi;
                    x2li = satrec.dsvalues.xli + satrec.dsvalues.xli;
                    xndt = d2201 * Math.sin(x2omi + satrec.dsvalues.xli - g22)
                            + d2211 * Math.sin(satrec.dsvalues.xli - g22)
                            + d3210
                            * Math.sin(xomi + satrec.dsvalues.xli - g32)
                            + d3222
                            * Math.sin(-xomi + satrec.dsvalues.xli - g32)
                            + d4410 * Math.sin(x2omi + x2li - g44) + d4422
                            * Math.sin(x2li - g44) + d5220
                            * Math.sin(xomi + satrec.dsvalues.xli - g52)
                            + d5232
                            * Math.sin(-xomi + satrec.dsvalues.xli - g52)
                            + d5421 * Math.sin(xomi + x2li - g54) + d5433
                            * Math.sin(-xomi + x2li - g54);
                    xldot = satrec.dsvalues.xni + xfact;
                    xnddt = d2201
                            * Math.cos(x2omi + satrec.dsvalues.xli - g22)
                            + d2211
                            * Math.cos(satrec.dsvalues.xli - g22)
                            + d3210
                            * Math.cos(xomi + satrec.dsvalues.xli - g32)
                            + d3222
                            * Math.cos(-xomi + satrec.dsvalues.xli - g32)
                            + d5220
                            * Math.cos(xomi + satrec.dsvalues.xli - g52)
                            + d5232
                            * Math.cos(-xomi + satrec.dsvalues.xli - g52)
                            + 2.0
                            * (d4410 * Math.cos(x2omi + x2li - g44) + d4422
                                    * Math.cos(x2li - g44) + d5421
                                    * Math.cos(xomi + x2li - g54) + d5433
                                    * Math.cos(-xomi + x2li - g54));
                    xnddt = xnddt * xldot;
                }

                /* ----------------------- integrator ------------------- */
                if (iretn == 381) {
                    satrec.dsvalues.xli = satrec.dsvalues.xli + xldot * delt
                            + xndt * step2;
                    satrec.dsvalues.xni = satrec.dsvalues.xni + xndt * delt
                            + xnddt * step2;
                    satrec.dsvalues.atime = satrec.dsvalues.atime + delt;
                }
            } // while iretn = 381

            nm = satrec.dsvalues.xni + xndt * ft + xnddt * ft * ft * 0.5;
            xl = satrec.dsvalues.xli + xldot * ft + xndt * ft * ft * 0.5;
            if (irez != 1) {
                mm = xl - 2.0 * omegam + 2.0 * theta;
                dndt = nm - no;
            } else {
                mm = xl - omegam - argpm + theta;
                dndt = nm - no;
            }

            nm = no + dndt;
        }

        if (help == 'y') {

            System.out.printf("%84s\n",
                    " ------------------after dspace--------------- ");
            System.out.printf(" inputs : \n");
            System.out.printf(
                    "%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "irez", irez, "d2201", d2201, "d2211", d2211, "d3210",
                    d3210, "d3222", d3222, "d4410", d4410);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "d4422", d4422, "d5220", d5220, "d5232", d5232, "d5421",
                    d5421, "d5433", d5433, "dedt", dedt);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "del1", del1, "del2", del2, "del3", del3, "didt", didt,
                    "dmdt", dmdt, "dnodt", dnodt);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "domdt", domdt, "argpo", argpo, "argpdot", argpdot, "t", t,
                    "tc", tc, "gsto", gsto);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "xfact", xfact,
                    "xlamo", xlamo, "no", no);
            System.out.printf(" in / out : \n");
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "atime", satrec.dsvalues.atime, "em", em, "argpm", argpm,
                    "inclm", inclm, "xli", satrec.dsvalues.xli, "mm", mm);
            System.out.printf("%7s%15.9f%7s%15.9f\n", "xni",
                    satrec.dsvalues.xni, "omegam", omegam);
            System.out.printf(" outputs : \n");
            System.out.printf("%7s%15.9f%7s%15.9f\n", "dndt", dndt, "nm", nm);

        }
    }

    /**
     * 
     * @param jdut1
     * @return
     */
    private double gstime(double jdut1) {
        double deg2rad = pi / 180.0;
        double temp, tut1;

        tut1 = (jdut1 - 2451545.0) / 36525.0;
        temp = -6.2e-6 * tut1 * tut1 * tut1 + 0.093104 * tut1 * tut1
                + (876600.0 * 3600 + 8640184.812866) * tut1 + 67310.54841; // sec
        temp = modfunc(temp * deg2rad / 240.0, twopi); // 360/86400 = 1/240, to
        // deg, to rad

        /* ------------------------ check quadrants --------------------- */
        if (temp < 0.0)
            temp += twopi;

        return temp;
    }

    /**
     * 
     * @param card1
     * @param card2
     */
    public void init(String card1, String card2) throws SatElsetException,
    ObjectDecayed {

        // Initialize the SGP4 structures
        twoline2rv(card1, card2);

    }

    /*-----------------------------------------------------------------------------
     *
     *                           procedure initl
     *
     *  this procedure initializes the spg4 propagator.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    ecco        - eccentricity                           0.0 - 1.0
     *    epoch       - epoch time in days from jan 0, 1950. 0 hr
     *    inclo       - inclination of satellite
     *    no          - mean motion of satellite
     *    satn        - satellite number
     *
     *  outputs       :
     *    ainv        - 1.0 / a
     *    ao          - semi major axis
     *    con41       -
     *    con42       - 1.0 - 5.0 cos(i)
     *    cosio       - cosine of inclination
     *    cosio2      - cosio squared
     *    einv        - 1.0 / e
     *    eccsq       - eccentricity squared
     *    method      -
     *    omeosq      - 1.0 - ecco * ecco
     *    posq        - semi-parameter squared
     *    rp          - radius of perigee
     *    rteosq      - square root of (1.0 - ecco*ecco)
     *    sinio       - sine of inclination
     *    gsto        - gst at time of observation               rad
     *    no          - mean motion of satellite
     *
     *  locals        :
     *    ak          -
     *    d1          -
     *    del         -
     *    adel        -
     *    po          -
     *
     *  coupling      :
     *    none.
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    private void initl(int satn, double ecco, double epoch, double inclo) throws ObjectDecayed
     {
        /* --------------------- local variables ------------------------ */
        double ak, d1, del, adel, po, x2o3, j2, xke;

        /* -------------------- wgs-72 earth constants ----------------- */
        xke = 7.43669161331734132e-2;
        j2 = 1.082616e-3;
        x2o3 = 2.0 / 3.0;

        /* ------------- calculate auxillary epoch quantities ---------- */
        eccsq = ecco * ecco;
        omeosq = 1.0 - eccsq;
        rteosq = Math.sqrt(omeosq);
        cosio = Math.cos(inclo);
        cosio2 = cosio * cosio;

        /* ------------------ un-kozai the mean motion ----------------- */
        /* make sure .elm file has correct line endings! */
        ak = Math.pow(xke / satrec.no, x2o3);
        d1 = 0.75 * j2 * (3.0 * cosio2 - 1.0) / (rteosq * omeosq);
        del = d1 / (ak * ak);
        adel = ak
                * (1.0 - del * del - del
                        * (1.0 / 3.0 + 134.0 * del * del / 81.0));
        del = d1 / (adel * adel);
        satrec.no = satrec.no / (1.0 + del);

        ao = Math.pow(xke / satrec.no, x2o3);
        sinio = Math.sin(inclo);
        po = ao * omeosq;
        con42 = 1.0 - 5.0 * cosio2;
        satrec.nevalues.con41 = -con42 - cosio2 - cosio2;
        ainv = 1.0 / ao; // FIXME is this used?
        einv = 1.0 / ecco;
        posq = po * po;
        rp = ao * (1.0 - ecco);
        satrec.nevalues.method = 0;
        if (rp < 1.0){
          // System.out.printf(" *** satn " + satn
          // + " epoch elts sub-orbital ***\n");
            throw new ObjectDecayed("Sgp4Unit.initl Fatal SGP4 error [satn: " + satn
                    + " epoch elts sub-orbital]");
        }
        satrec.dsvalues.gsto = gstime(epoch + 2433281.5);

 
        if (help == 'y')
        {
            System.out
                    .printf(" ------------------after initl  :---------------");
            System.out.printf("    inputs : \n");
            System.out.printf("satn " + satn + " yr ecco " + ecco + " epoch "
                    + epoch + " inclo " + inclo);
            System.out.printf("    in/out : \n");
            System.out.printf("no " + satrec.no);
            System.out.printf("\n    outputs : \n");
            System.out.printf("method " + satrec.nevalues.method + " ainv "
                    + ainv + " ao " + ao + " con41 " + satrec.nevalues.con41
                    + " con42 " + con42 + " cosio " + cosio + "\n");
            System.out.printf("%7s%15.9f\n", "cosio2", cosio2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "einv", einv, "eccsq", eccsq, "omeosq", omeosq, "posq",
                    posq, "rp", rp, "rteosq", rteosq);
            System.out.printf("%7s%15.9f%7s%15.9f\n", "sinio", sinio, "gsto",
                    satrec.dsvalues.gsto);
        }
    }

    /*------------------------------------------------------------------------------
     |
     |                           procedure julianday
     |
     |  this procedure finds the julian date given the year, month, day, and time.
     |    the julian date is defined by each elapsed day since noon, jan 1, 4713 bc.
     |
     |  algorithm     : calculate the answer in one step for efficiency
     |
     |  author        : david vallado                  303-344-6037    1 mar 2001
     |
     |  inputs          description                    range / units
     |    year        - year                           1900 .. 2100
     |    mon         - month                          1 .. 12
     |    day         - day                            1 .. 28,29,30,31
     |    hr          - universal time hour            0 .. 23
     |    min         - universal time min             0 .. 59
     |    sec         - universal time sec             0.0 .. 59.999
     |    whichtype   - julian or gregorian calender   'j' or 'g'
     |
     |  outputs       :
     |    jd          - julian date                    days from 4713 bc
     |
     |  locals        :
     |    b           - var to aid gregorian dates
     |
     |  coupling      :
     |    none.
     |
     |  references    :
     |    vallado       2001, 186-188, alg 14, ex 3-14
     |
     -----------------------------------------------------------------------------*/
    private double julianday(int year, int mon, int inday, int hr, int min,
            double sec) {
        double jd = 367.0 * year
                - (int) ((7 * (year + (int) ((mon + 9) / 12))) * 0.25)
                + (int) (275 * mon / 9) + inday + 1721013.5
                + ((sec / 60.0 + min) / 60.0 + hr) / 24.0; // ut in days
        // - 0.5*sgn(100.0*year + mon - 190002.5) + 0.5;
        return jd;
    }

    /**
     * 
     * @param x
     * @param y
     * @return
     */
    private double modfunc(double x, double y) {
        if (y != 0) // if (y)
            return x - (int) (x / y) * y;

        return 0;
    }

    /*------------------------------------------------------------------------------
     |
     |                           procedure newtonm
     |
     |  this procedure performs the newton rhapson iteration to find the
     |    eccentric anomaly given the mean anomaly.  the true anomaly is also
     |    calculated.
     |
     |  author        : david vallado                  303-344-6037    1 mar 2001
     |
     |  inputs          description                    range / units
     |    ecc         - eccentricity                   0.0 to
     |    m           - mean anomaly                   -2pi to 2pi rad
     |
     |  outputs       :
     |    e0          - eccentric anomaly              0.0 to 2pi rad
     |    nu          - true anomaly                   0.0 to 2pi rad
     |
     |  locals        :
     |    e1          - eccentric anomaly, next value  rad
     |    sinv        - sine of nu
     |    cosv        - cosine of nu
     |    ktr         - index
     |    r1r         - cubic roots - 1 to 3
     |    r1i         - imaginary component
     |    r2r         -
     |    r2i         -
     |    r3r         -
     |    r3i         -
     |    s           - variables for parabolic solution
     |    w           - variables for parabolic solution
     |
     |  coupling      :
     |    atan2       - arc tangent function which also resloves quadrants
     |    cubic       - solves a cubic polynomial
     |    power       - raises a base number to an arbitrary power
     |    sinh        - hyperbolic sine
     |    cosh        - hyperbolic cosine
     |    sgn         - returns the sign of an argument
     |
     |  references    :
     |    vallado       2001, 72-75, alg 2, ex 2-1
     |
     -----------------------------------------------------------------------------*/
    private void newtonm(double ecc, double m, double e0, double nu) {
        int numiter = 50;
        double small = 0.00000001; // small value for tolerances

        double e1, sinv, cosv, r1r = 0.0;
        int ktr;

        /* -------------------------- hyperbolic ----------------------- */
        if ((ecc - 1.0) > small) {
            /* ------------ initial guess ------------- */
            if (ecc < 1.6)
                if (((m < 0.0) && (m > -pi)) || (m > pi))
                    e0 = m - ecc;
                else
                    e0 = m + ecc;
            else if ((ecc < 3.6) && (Math.abs(m) > pi)) // just edges)
                e0 = m - sgn(m) * ecc;
            else
                e0 = m / (ecc - 1.0); // best over 1.8 in middle
            ktr = 1;
            e1 = e0
                    + ((m - ecc * Math.sinh(e0) + e0) / (ecc * Math.cosh(e0) - 1.0));
            while ((Math.abs(e1 - e0) > small) && (ktr <= numiter)) {
                e0 = e1;
                e1 = e0
                        + ((m - ecc * Math.sinh(e0) + e0) / (ecc
                                * Math.cosh(e0) - 1.0));
                ktr++;
            }
            /* --------- find true anomaly ----------- */
            sinv = -(Math.sqrt(ecc * ecc - 1.0) * Math.sinh(e1))
                    / (1.0 - ecc * Math.cosh(e1));
            cosv = (Math.cosh(e1) - ecc) / (1.0 - ecc * Math.cosh(e1));
            nu = Math.atan2(sinv, cosv);
        } else {
            /* ---------------------- parabolic ------------------------- */
            if (Math.abs(ecc - 1.0) < small) {
                // kbn cubic(1.0 / 3.0, 0.0, 1.0, -m, r1r, r1i, r2r, r2i, r3r,
                // r3i);
                e0 = r1r;
                // kbn if (fileout != null)
                // fprintf(fileout, "roots %11.7f %11.7f %11.7f %11.7f %11.7f
                // %11.7f\n",
                // r1r, r1i, r2r, r2i, r3r, r3i);
                /*
                 * s = 0.5 * (halfpi - atan(1.5 * m)); w = atan(power(tan(s),
                 * 1.0 / 3.0)); e0 = 2.0 * cot(2.0* w );
                 */
                ktr = 1;
                nu = 2.0 * Math.atan(e0);
            } else {
                /* --------------------- elliptical --------------------- */
                if (ecc > small) {
                    /* ------------ initial guess ------------- */
                    if (((m < 0.0) && (m > -pi)) || (m > pi))
                        e0 = m - ecc;
                    else
                        e0 = m + ecc;
                    ktr = 1;
                    e1 = e0 + (m - e0 + ecc * Math.sin(e0))
                            / (1.0 - ecc * Math.cos(e0));
                    while ((Math.abs(e1 - e0) > small) && (ktr <= numiter)) {
                        ktr++;
                        e0 = e1;
                        e1 = e0 + (m - e0 + ecc * Math.sin(e0))
                                / (1.0 - ecc * Math.cos(e0));
                    }
                    /* --------- find true anomaly ----------- */
                    sinv = (Math.sqrt(1.0 - ecc * ecc) * Math.sin(e1))
                            / (1.0 - ecc * Math.cos(e1));
                    cosv = (Math.cos(e1) - ecc) / (1.0 - ecc * Math.cos(e1));
                    nu = Math.atan2(sinv, cosv);
                } else {
                    /* --------------------- circular --------------------- */
                    ktr = 0;
                    nu = m;
                    e0 = m;
                }
            }
        }
        if (ktr > numiter)
            System.out.printf(
                    "newton rhapson not converged in %3d iterations\n", numiter);
    }

    /**
     * 
     * @param startYear
     * @param startDay
     * @return
     */
    public Sgp4Data runSgp4(int startYear, double startDay)
            throws ObjectDecayed {

        if (startYear < 1900) {
            if (startYear < 50)
                startYear = startYear + 2000;
            else
                startYear = startYear + 1900;
        }

        satrec.srtime = (startYear - 1950) * 365 + (startYear - 1949) / 4
                + startDay;

        // need to check if there is an epoch time
        // set the time to calculate, use the epoch time generated from the
        // initialization
        // based on days since 1950 and minutes from epoch
        satrec.nevalues.t = (satrec.srtime - satrec.eptime) * 1440.0;

        Sgp4Data outDat = sgp4();

        return outDat;
    }

    /**
     * Run SGP4 for a period of time
     * 
     * @param card1
     * @param card2
     * @param startYear
     * @param startDay
     * @param stopYear
     * @param stopDay
     * @param step
     * @return Vector of Sgp4Data elements
     */
    public Vector<Sgp4Data> runSgp4(String card1, String card2, int startYear,
            double startDay, int stopYear, double stopDay, double step)
            throws SatElsetException, ObjectDecayed {

        if (startYear < 1900) {
            if (startYear < 50)
                startYear = startYear + 2000;
            else
                startYear = startYear + 1900;
        }

        if (stopYear < 1900) {
            if (stopYear < 50)
                stopYear = stopYear + 2000;
            else
                stopYear = stopYear + 1900;
        }

        satrec.srtime = (startYear - 1950) * 365 + (startYear - 1949) / 4
                + startDay;

        satrec.sptime = (stopYear - 1950) * 365 + (stopYear - 1949) / 4
                + stopDay;
        satrec.deltamin = step;

        Vector<Sgp4Data> sgp4Results = new Vector<Sgp4Data>();

        // Initialize the SGP4 structures
        twoline2rv(card1, card2);

        // set the time to calculate, use the epoch time generated from the
        // initialization
        // based on days since 1950 and minutes from epoch
        satrec.nevalues.t = (satrec.srtime - satrec.eptime) * 1440.0;
        
        double stopTime = (satrec.sptime - satrec.eptime) * 1440.0 + satrec.deltamin;

        boolean endFlg = false;
        
       // increment by deltamin until we hit the stop time
       while ((satrec.nevalues.t < stopTime) && (satrec.error == 0) && !endFlg) {

            // Capture the last point
            if (satrec.nevalues.t > stopTime){
                satrec.nevalues.t = stopTime;
                endFlg = true;
            }

            sgp4Results.addElement(sgp4());

            satrec.nevalues.t = satrec.nevalues.t + satrec.deltamin;
        }

        return sgp4Results;
    }

    /**
     * 
     * @param x
     * @return
     */
    private double sgn(double x) {
        if (x != 0) // if (x)
            return Math.abs(x) / x;

        return 0;
    }

    /*-----------------------------------------------------------------------------
     *
     *                             procedure sgp4
     *
     *  this procedure is the sgp4 spadoc compatible prediction model from
     *    spacetrack report #3.  this is an updated version of sgp4 and sdp4, which
     *    were originally published separately in spacetrack report #3. the version
     *    follows the nasa release on the internet. there are some fixes that are
     *    added to the em tolerances.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    bstar       -
     *    ecco        - initial eccentricity
     *    argpo       - initial argument of perigee
     *    inclo       - initial inclination
     *    nevalues.t  - time in minutes from the epoch of the satellite (set outside)
     *    mo          - initial mean anomaly
     *    no          - initial mean motion
     *    omegao      - initial long of ascending node
     *    nevalues    - near earth common values for subsequent calls
     *    dsvalues    - deep space common values for calls
     *
     *  outputs       :
     *    x           - x component of position                          er
     *    y           - y component of position
     *    z           - z component of position
     *    xdot        - x component of velocity                          er/min
     *    ydot        - y component of velocity
     *    zdot        - z component of velocity
     *
     *  locals        :
     *    am          -
     *    axnl, aynl        -
     *    betal       -
     *    cosim       -
     *    cosomm      -
     *    cnod        -
     *    cos2u       -
     *    coseo1      -
     *    cosi        -
     *    cosip       -
     *    cosisq      -
     *    cossu       -
     *    cosu        -
     *    delm        -
     *    delomg      -
     *    dndt        -
     *    em          -
     *    emsq        -
     *    ecose       -
     *    el2         -
     *    eo1         -
     *    ep          -
     *    esine       -
     *    argpm       -
     *    argpp       -
     *    argpdf      -
     *    pl          -
     *    r           -
     *    rtemsq      -
     *    rdot        -
     *    rdotl       -
     *    rl          -
     *    rvdot       -
     *    rvdotl      -
     *    sinim       -
     *    sinomm      -
     *    sin2u       -
     *    sineo1      -
     *    sini        -
     *    sinip       -
     *    sinu        -
     *    snod        -
     *    su          -
     *    t2, t3, t4          -
     *    tc          -
     *    tem5        -
     *    temp, temp1, temp2, tempa, tempe, templ
     *    u           -
     *    ux, uy, uz          -
     *    vx, vy, vz          -
     *    inclm       - inclination
     *    mm          - mean anomaly
     *    nm          - mean motion
     *    omegam      - longi of ascending node
     *    xinc        -
     *    xincp       -
     *    xl          -
     *    xlm         -
     *    mp          -
     *    xmdf        -
     *    xmx         -
     *    xmy         -
     *    omegadf     -
     *    xnode       -
     *    omegap      -
     *    np          -
     *
     *  coupling      :
     *    dpper
     *    dpspace
     *
     *  references    :
     *    norad spacetrack report #3
     *
     ----------------------------------------------------------------------------*/
    public Sgp4Data sgp4() throws ObjectDecayed {

        double am, axnl, aynl, betal, cnod, cos2u, coseo1 = 0.0;
        double cosi, cosip, cosisq, cossu, cosu, delm, delomg;
        double ecose, el2, eo1, esine;
        // double argpp, argpm, em, ep;
        double cosim, emsq, sinim;
        double argpdf, pl, mrt;
        double mvt, rdotl, rl, rvdot, rvdotl, sin2u, sineo1 = 0.0;
        double sini, sinip, sinsu, sinu, snod, su, t2, t3, t4, tem5, temp;
        double temp1, temp2, tempa, tempe, templ;
        double u, ux, uy, uz, vx, vy, vz;
        // double inclm;
        // double mm, nm, omegam, dndt = 0.0;
        double xinc;
        double xl, xlm;
        // double mp;
        double xmdf, xmx, xmy, omegadf;
        double xnode;
        double tc, x2o3, j2, j3;
        double xke, j3oj2;
        int ktr;

        // FIXME WGS-84
        /* -------------------- wgs-72 earth constants ----------------- */
        /* ------------------ set mathematical constants --------------- */
        x2o3 = 2.0 / 3.0;
        xke = 7.43669161331734132e-2;
        j2 = 1.082616e-3;
        j3 = -2.53881e-6;
        j3oj2 = j3 / j2;

        /* --------------------- clear sgp4 error flag ----------------- */
        satrec.error = 0;

        /* ------- update for secular gravity and atmospheric drag ----- */
        xmdf = satrec.mo + satrec.nevalues.mdot * satrec.nevalues.t;
        argpdf = satrec.argpo + satrec.nevalues.argpdot * satrec.nevalues.t;
        omegadf = satrec.omegao + satrec.nevalues.omegadot * satrec.nevalues.t;
        argpm = argpdf;
        mm = xmdf;
        t2 = satrec.nevalues.t * satrec.nevalues.t;
        omegam = omegadf + satrec.nevalues.omegacf * t2;
        tempa = 1.0 - satrec.nevalues.cc1 * satrec.nevalues.t;
        tempe = satrec.bstar * satrec.nevalues.cc4 * satrec.nevalues.t;
        templ = satrec.nevalues.t2cof * t2;

        if (satrec.nevalues.isimp != 1) {
            delomg = satrec.nevalues.omgcof * satrec.nevalues.t;
            delm = satrec.nevalues.xmcof
                    * (Math
                            .pow((1.0 + satrec.nevalues.eta * Math.cos(xmdf)),
                                    3) - satrec.nevalues.delmo);
            temp = delomg + delm;
            mm = xmdf + temp;
            argpm = argpdf - temp;
            t3 = t2 * satrec.nevalues.t;
            t4 = t3 * satrec.nevalues.t;
            tempa = tempa - satrec.nevalues.d2 * t2 - satrec.nevalues.d3 * t3
                    - satrec.nevalues.d4 * t4;
            tempe = tempe + satrec.bstar * satrec.nevalues.cc5
                    * (Math.sin(mm) - satrec.nevalues.sinmao);
            templ = templ
                    + satrec.nevalues.t3cof
                    * t3
                    + t4
                    * (satrec.nevalues.t4cof + satrec.nevalues.t
                            * satrec.nevalues.t5cof);
        }

        nm = satrec.no;
        em = satrec.ecco;
        inclm = satrec.inclo;
        if (satrec.nevalues.method == 2) {
            tc = satrec.nevalues.t;
            dspace(satrec.dsvalues.irez, satrec.dsvalues.d2201,
                    satrec.dsvalues.d2211, satrec.dsvalues.d3210,
                    satrec.dsvalues.d3222, satrec.dsvalues.d4410,
                    satrec.dsvalues.d4422, satrec.dsvalues.d5220,
                    satrec.dsvalues.d5232, satrec.dsvalues.d5421,
                    satrec.dsvalues.d5433, satrec.dsvalues.dedt,
                    satrec.dsvalues.del1, satrec.dsvalues.del2,
                    satrec.dsvalues.del3, satrec.dsvalues.didt,
                    satrec.dsvalues.dmdt, satrec.dsvalues.dnodt,
                    satrec.dsvalues.domdt, satrec.argpo,
                    satrec.nevalues.argpdot, satrec.nevalues.t, tc,
                    satrec.dsvalues.gsto, satrec.dsvalues.xfact,
                    satrec.dsvalues.xlamo, satrec.no);
            // add check here for incl too - no.
        }

        // Check if mean motion is less than or equal to zero
        if (nm <= 0.0)
        {
            // throw an exception only if this is a fatal condition
            // which may result in a divide by zero error, otherwise
            // try and recover
            satrec.error = 2;
            // System.out.println("ERROR mean motion is less than zero [nm: " +
            // nm+"]");
            throw new ObjectDecayed("Sgp4Unit.sgp4 ERROR mean motion is less than zero [nm: " + nm+"]");
        }
        
        am = Math.pow((xke / nm), x2o3) * tempa * tempa;
        nm = xke / Math.pow(am, 1.5);
        // subtract drag effects on the eccentricity
        em = em - tempe;

        // Check for eccentricity being out of bounds
        if ((em >= 1.0) || (em < -0.001) || (am < 0.95))
        {
            satrec.error = 1;
          // System.out.println("ERROR eccentricity out of bounds"+
          // " [em: " + em+"] [am: "+am+"]");
            throw new ObjectDecayed("Sgp4Unit.sgp4 ERROR eccentricity out of bounds"+
                    " [em: " + em+"] [am: "+am+"]");
        }
        
        // If it is less than zero, try and correct by making eccentricity a
        // small value
        if (em < 0.0)
            em = 1.0e-6;
        mm = mm + satrec.no * templ;
        xlm = mm + argpm + omegam;
        emsq = em * em;
        temp = 1.0 - emsq;
        // rtemsq = sqrt(temp);
        omegam = modfunc(omegam, twopi);
        argpm = modfunc(argpm, twopi);
        xlm = modfunc(xlm, twopi);
        mm = modfunc(xlm - argpm - omegam, twopi);
        // if (mm < 0.0)
        // mm = mm + twopi;

        /* ----------------- compute extra mean quantities ------------- */
        sinim = Math.sin(inclm);
        cosim = Math.cos(inclm);

        /* -------------------- add lunar-solar periodics -------------- */
        // np = nm;
        // FIXME send to dpper
        satrec.ep = em;
        satrec.xincp = inclm;
        satrec.argpp = argpm;
        satrec.omegap = omegam;
        satrec.mp = mm;
        sinip = sinim;
        cosip = cosim;
        if (satrec.nevalues.method == 2) {

            dpper(satrec.dsvalues.e3, satrec.dsvalues.ee2, satrec.dsvalues.peo,
                    satrec.dsvalues.pgho, satrec.dsvalues.pho,
                    satrec.dsvalues.pinco, satrec.dsvalues.plo,
                    satrec.dsvalues.se2, satrec.dsvalues.se3,
                    satrec.dsvalues.sgh2, satrec.dsvalues.sgh3,
                    satrec.dsvalues.sgh4, satrec.dsvalues.sh2,
                    satrec.dsvalues.sh3, satrec.dsvalues.si2,
                    satrec.dsvalues.si3, satrec.dsvalues.sl2,
                    satrec.dsvalues.sl3, satrec.dsvalues.sl4,
                    satrec.nevalues.t, satrec.dsvalues.xgh2,
                    satrec.dsvalues.xgh3, satrec.dsvalues.xgh4,
                    satrec.dsvalues.xh2, satrec.dsvalues.xh3,
                    satrec.dsvalues.xi2, satrec.dsvalues.xi3,
                    satrec.dsvalues.xl2, satrec.dsvalues.xl3,
                    satrec.dsvalues.xl4, satrec.dsvalues.zmol,
                    satrec.dsvalues.zmos, 0);
            // in ep, xincp, omegap, argpp, mp
            // out double& ep, double& inclp, double& omegap, double& argpp,
            // double& mp

            // Correct for negative inclination
            if (satrec.xincp < 0.0)
            {
                satrec.xincp = -satrec.xincp; 
                satrec.omegap = satrec.omegap + pi;
                satrec.argpp = satrec.argpp - pi;
            }
            
            // Another eccentricity check
            if ((satrec.ep < 0.0) || (satrec.ep > 1.0))
            {
              // System.out.println("ERROR eccentricity out of bounds [ep: " +
                // satrec.ep+"]");
                satrec.error = 3;
                throw new ObjectDecayed("Sgp4Unit.sgp4 ERROR eccentricity out of bounds [ep: " + satrec.ep+"]");
            }
        }

        /* -------------------- long period periodics ------------------ */
        if (satrec.nevalues.method == 2) {
            sinip = Math.sin(satrec.xincp);
            cosip = Math.cos(satrec.xincp);
            satrec.nevalues.aycof = -0.5 * j3oj2 * sinip;
            satrec.nevalues.xlcof = -0.25 * j3oj2 * sinip * (3.0 + 5.0 * cosip)
                    / (1.0 + cosip);
        }
        axnl = satrec.ep * Math.cos(satrec.argpp);
        temp = 1.0 / (am * (1.0 - satrec.ep * satrec.ep));
        aynl = satrec.ep * Math.sin(satrec.argpp) + temp
                * satrec.nevalues.aycof;
        xl = satrec.mp + satrec.argpp + satrec.omegap + temp
                * satrec.nevalues.xlcof * axnl;

        /* --------------------- solve kepler's equation --------------- */
        u = modfunc(xl - satrec.omegap, twopi);
        eo1 = u;
        tem5 = 9999.9;
        ktr = 1;
        // sgp4fix for kepler iteration
        // the following iteration needs better limits on corrections

        while ((Math.abs(tem5) >= 1.0e-12) && (ktr <= 10)) {
            sineo1 = Math.sin(eo1);
            coseo1 = Math.cos(eo1);
            tem5 = 1.0 - coseo1 * axnl - sineo1 * aynl;
            tem5 = (u - aynl * coseo1 + axnl * sineo1 - eo1) / tem5;
            if (Math.abs(tem5) >= 0.95)
                tem5 = tem5 > 0.0 ? 0.95 : -0.95; // PSC - crude fix
            // for convergance.
            eo1 = eo1 + tem5;
            ktr = ktr + 1;
        }

        /* ------------- short period preliminary quantities ----------- */
        ecose = axnl * coseo1 + aynl * sineo1;
        esine = axnl * sineo1 - aynl * coseo1;
        el2 = axnl * axnl + aynl * aynl;
        pl = am * (1.0 - el2);
        
        if (pl < 0.0) {
            // This error results in no data generated
           // System.out.println("FATAL Error [pl: "+ pl+"]");
            satrec.error = 4;
            throw new ObjectDecayed("Sgp4Unit.sgp4 ERROR [pl: "+ pl+"]");
        } else {
            rl = am * (1.0 - ecose);
            rdotl = Math.sqrt(am) * esine / rl;
            rvdotl = Math.sqrt(pl) / rl;
            betal = Math.sqrt(1.0 - el2);
            temp = esine / (1.0 + betal);
            sinu = am / rl * (sineo1 - aynl - axnl * temp);
            cosu = am / rl * (coseo1 - axnl + aynl * temp);
            su = Math.atan2(sinu, cosu);
            sin2u = (cosu + cosu) * sinu;
            cos2u = 1.0 - 2.0 * sinu * sinu;
            temp = 1.0 / pl;
            temp1 = 0.5 * j2 * temp;
            temp2 = temp1 * temp;

            /* -------------- update for short period periodics ------------ */
            if (satrec.nevalues.method == 2) {
                cosisq = cosip * cosip;
                satrec.nevalues.con41 = 3.0 * cosisq - 1.0;
                satrec.nevalues.x1mth2 = 1.0 - cosisq;
                satrec.nevalues.x7thm1 = 7.0 * cosisq - 1.0;
            }
            mrt = rl * (1.0 - 1.5 * temp2 * betal * satrec.nevalues.con41)
                    + 0.5 * temp1 * satrec.nevalues.x1mth2 * cos2u;
            su = su - 0.25 * temp2 * satrec.nevalues.x7thm1 * sin2u;
            xnode = satrec.omegap + 1.5 * temp2 * cosip * sin2u;
            xinc = satrec.xincp + 1.5 * temp2 * cosip * sinip * cos2u;
            mvt = rdotl - nm * temp1 * satrec.nevalues.x1mth2 * sin2u / xke;
            rvdot = rvdotl
                    + nm
                    * temp1
                    * (satrec.nevalues.x1mth2 * cos2u + 1.5 * satrec.nevalues.con41)
                    / xke;

            /* --------------------- orientation vectors ------------------- */
            sinsu = Math.sin(su);
            cossu = Math.cos(su);
            snod = Math.sin(xnode);
            cnod = Math.cos(xnode);
            sini = Math.sin(xinc);
            cosi = Math.cos(xinc);
            xmx = -snod * cosi;
            xmy = cnod * cosi;
            ux = xmx * sinsu + cnod * cossu;
            uy = xmy * sinsu + snod * cossu;
            uz = sini * sinsu;
            vx = xmx * cossu - cnod * sinsu;
            vy = xmy * cossu - snod * sinsu;
            vz = sini * cossu;

            /* ------------------- position and velocity ------------------- */

            satrec.r[0] = mrt * ux;
            satrec.r[1] = mrt * uy;
            satrec.r[2] = mrt * uz;
            satrec.v[0] = mvt * ux + rvdot * vx;
            satrec.v[1] = mvt * uy + rvdot * vy;
            satrec.v[2] = mvt * uz + rvdot * vz;

        }

        if (satrec.error > 0) {

            // Decay errors
            // satrec.error = 2; mean motion out of bounds
            // satrec.error = 1 eccentricity out of bounds
            // satrec.error = 3 eccentricity out of bounds
            
          // System.out.printf("*** error: t:= %f *** code = %3d\n",
          // satrec.nevalues.t, satrec.error);

            if (satrec.error == 4) {
                // no data generated
                throw new ObjectDecayed("Sgp4Unit.sgp4 Fatal SGP4 error [pl: "+ pl+"]");
            }
        }

        if (help == 'y') {
            System.out.printf("%84s\n",
                    "------------------after sgp4   :---------------");
            System.out.printf("    inputs : \n");
            System.out.printf(
                    "%7s%15d%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "isimp", satrec.nevalues.isimp, "method",
                    satrec.nevalues.method, "aycof", satrec.nevalues.aycof,
                    "bstar", satrec.bstar, "con41", satrec.nevalues.con41,
                    "cc1", satrec.nevalues.cc1);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "cc4", satrec.nevalues.cc4, "cc5", satrec.nevalues.cc5,
                    "d2", satrec.nevalues.d2, "d3", satrec.nevalues.d3, "d4",
                    satrec.nevalues.d4, "delmo", satrec.nevalues.delmo);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "ecco", satrec.ecco, "eta", satrec.nevalues.eta, "argpo",
                    satrec.argpo, "argpdot", satrec.nevalues.argpdot, "omgcof",
                    satrec.nevalues.omgcof, "sinmao", satrec.nevalues.sinmao);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "t", satrec.nevalues.t, "t2cof", satrec.nevalues.t2cof,
                    "t3cof", satrec.nevalues.t3cof, "t4cof",
                    satrec.nevalues.t4cof, "t5cof", satrec.nevalues.t5cof,
                    "x1mth2", satrec.nevalues.x1mth2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "x7thm1", satrec.nevalues.x7thm1, "inclo", satrec.inclo,
                    "mo", satrec.mo, "mdot", satrec.nevalues.mdot, "xno",
                    satrec.no, "omegao", satrec.omegao);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "omegadt", satrec.nevalues.omegadot, "xlcof",
                    satrec.nevalues.xlcof, "xmcof", satrec.nevalues.xmcof,
                    "omegacf", satrec.nevalues.omegacf);
            System.out.printf("    outputs : \n");
            System.out
                    .printf(
                            "%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                            "error", satrec.error, "x", satrec.r[0], "y",
                            satrec.r[1], "z", satrec.r[2], "xdot", satrec.v[0],
                            "ydot", satrec.v[1], "zdot", satrec.v[2]);
            System.out.printf("    extra inputs for ds : \n");
            System.out.printf("%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "irez", satrec.dsvalues.irez, "d2201",
                    satrec.dsvalues.d2201, "d2211", satrec.dsvalues.d2211,
                    "d3210", satrec.dsvalues.d3210, "d3222",
                    satrec.dsvalues.d3222);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "d4410", satrec.dsvalues.d4410, "d4422",
                    satrec.dsvalues.d4422, "d5220", satrec.dsvalues.d5220,
                    "d5232", satrec.dsvalues.d5232, "d5421",
                    satrec.dsvalues.d5421, "d5433", satrec.dsvalues.d5433);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "dedt", satrec.dsvalues.dedt, "del1", satrec.dsvalues.del1,
                    "del2", satrec.dsvalues.del2, "del3", satrec.dsvalues.del3,
                    "didt", satrec.dsvalues.didt, "dmdt", satrec.dsvalues.dmdt);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "dnodt", satrec.dsvalues.dnodt, "domdt",
                    satrec.dsvalues.domdt, "e3", satrec.dsvalues.e3, "ee2",
                    satrec.dsvalues.ee2, "peo", satrec.dsvalues.peo, "pgho",
                    satrec.dsvalues.pgho);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "pho", satrec.dsvalues.pho, "pinco", satrec.dsvalues.pinco,
                    "plo", satrec.dsvalues.plo, "se2", satrec.dsvalues.se2,
                    "se3", satrec.dsvalues.se3, "sgh2", satrec.dsvalues.sgh2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sgh3", satrec.dsvalues.sgh3, "sgh4", satrec.dsvalues.sgh4,
                    "sh2", satrec.dsvalues.sh2, "sh3", satrec.dsvalues.sh3,
                    "si2", satrec.dsvalues.si2, "si3", satrec.dsvalues.si3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sl2", satrec.dsvalues.sl2, "sl3", satrec.dsvalues.sl3,
                    "sl4", satrec.dsvalues.sl4, "gsto", satrec.dsvalues.gsto,
                    "xfact", satrec.dsvalues.xfact, "xgh2",
                    satrec.dsvalues.xgh2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xgh3", satrec.dsvalues.xgh3, "xgh4", satrec.dsvalues.xgh4,
                    "xh2", satrec.dsvalues.xh2, "xh3", satrec.dsvalues.xh3,
                    "xi2", satrec.dsvalues.xi2, "xi3", satrec.dsvalues.xi3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xl2", satrec.dsvalues.xl2, "xl3", satrec.dsvalues.xl3,
                    "xl4", satrec.dsvalues.xl4, "xlamo", satrec.dsvalues.xlamo,
                    "zmol", satrec.dsvalues.zmol, "zmos", satrec.dsvalues.zmos);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "atime",
                    satrec.dsvalues.atime, "xli", satrec.dsvalues.xli, "xni",
                    satrec.dsvalues.xni);
        }

        Sgp4Data data = new Sgp4Data(satrec.satnum);

        data.setPosn(new Vector3d(satrec.r[0], satrec.r[1], satrec.r[2]));
        data.setVel(new Vector3d(satrec.v[0], satrec.v[1], satrec.v[2]));

        return data;
    }

    /*-----------------------------------------------------------------------------
     *
     *                             procedure sgp4init
     *
     *  this procedure initializes variables for sgp4. the fix to conform to
     *    ver 3.01 consisted only of adding a var so the 'o elements would be
     *    passed back for deep space initialization cases.
     *
     *  author        : david vallado                  719-573-2600    1 mar 2001
     *
     *  inputs        :
     *    satn        - satellite number
     *    year        - year of observation                    1950-2049
     *    epoch       - epoch time in days from jan 0, 1950. 0 hr
     *
     *  outputs       :
     *    no          - mean motion
     *    omegao      - longitude of ascending node
     *    init        - flag for first pass 1-yes, 0-not first pass
     *    nevalues    - near earth common values for subsequent calls
     *    dsvalues    - deep space common values for calls
     *
     *  locals        :
     *    cnodm       -
     *    snodm       -
     *    cosim       -
     *    sinim       -
     *    cosomm      -
     *    sinomm      -
     *    cc1sq       -
     *    cc2         -
     *    cc3         -
     *    coef        -
     *    coef1       -
     *    cosio4      -
     *    day         -
     *    dndt        -
     *    em          -
     *    emsq        -
     *    eeta        -
     *    etasq       -
     *    gam         -
     *    argpm       -
     *    ndem        -
     *    inclm       - inclination
     *    mm          - mean anomaly
     *    nm          - mean motion
     *    perige      -
     *    pinvsq      -
     *    psisq       -
     *    qzms24      -
     *    rtemsq      -
     *    s1, s2, s3, s4, s5, s6, s7          -
     *    sfour       -
     *    ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
     *    sz1, sz2, sz3
     *    sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
     *    tc          -
     *    temp        -
     *    temp1, temp2, temp3       -
     *    tsi         -
     *    xpidot      -
     *    xhdot1      -
     *    z1, z2, z3          -
     *    z11, z12, z13, z21, z22, z23, z31, z32, z33         -
     *
     *  coupling      :
     *    initl       -
     *    dscom       -
     *    dpper       -
     *    dsinit      -
     *
     *  references    :
     *    norad spacetrack report no.3 "models for propagation of norad element sets"
     *              1 nov 1980:  felix r. hoots, ronald l. roehrich
     *
     ----------------------------------------------------------------------------*/
    private void sgp4init(int satn, int year, double epoch) throws ObjectDecayed{

        /* --------------------- local variables ------------------------ */
        double cc1sq = 0.0, cc2 = 0.0, cc3 = 0.0, coef = 0.0;
        double coef1 = 0.0, cosio4 = 0.0;
        double eeta = 0.0, etasq = 0.0;
        double perige = 0.0;
        double pinvsq = 0.0, psisq = 0.0, qzms24 = 0.0;
        double sfour = 0.0;
        ss1 = 0.0;
        ss2 = 0.0;
        ss3 = 0.0;
        ss4 = 0.0;
        ss5 = 0.0;
        ss6 = 0.0;
        ss7 = 0.0;
        sz1 = 0.0;
        sz2 = 0.0;
        sz3 = 0.0;
        sz11 = 0.0;
        sz12 = 0.0;
        sz13 = 0.0;
        sz21 = 0.0;
        sz22 = 0.0;
        sz23 = 0.0;
        sz31 = 0.0;
        sz32 = 0.0;
        sz33 = 0.0;
        z1 = 0.0;
        z2 = 0.0;
        z3 = 0.0;
        z11 = 0.0;
        z12 = 0.0;
        z13 = 0.0;
        z21 = 0.0;
        z22 = 0.0;
        z23 = 0.0;
        z31 = 0.0;
        z32 = 0.0;
        z33 = 0.0;
        double tc = 0.0, temp = 0.0, temp1 = 0.0, temp2 = 0.0;
        double temp3 = 0.0, tsi = 0.0, xpidot = 0.0, xhdot1 = 0.0;
        double qzms2t = 0.0, ss = 0.0;
        double radiusearthkm = 0.0, j2 = 0.0, j3oj2 = 0.0;
        double j4 = 0.0, x2o3 = 0.0;

        /* ------------------------ initialization --------------------- */

        /* ----------- set all deep space variables to zero ------------ */

        radiusearthkm = 6378.135;
        ss = 78.0 / radiusearthkm + 1.0;
        qzms2t = Math.pow(((120.0 - 78.0) / radiusearthkm), 4);
        j2 = 1.082616e-3;
        j3oj2 = -2.53881e-6 / j2;
        x2o3 = 2.0 / 3.0;
        j4 = -1.65597e-6;

        initl(satn, satrec.ecco, epoch, satrec.inclo);

        if ((omeosq >= 0.0) || (satrec.no >= 0.0)) {
            satrec.nevalues.isimp = 0;
            if (rp < (220.0 / radiusearthkm + 1.0))
                satrec.nevalues.isimp = 1;
            sfour = ss;
            qzms24 = qzms2t;
            perige = (rp - 1.0) * radiusearthkm;

            /* - for perigees below 156 km, s and qoms2t are altered - */
            if (perige < 156.0) {
                sfour = perige - 78.0;
                if (perige < 98.0)
                    sfour = 20.0;
                qzms24 = Math.pow(((120.0 - sfour) / radiusearthkm), 4.0);
                sfour = sfour / radiusearthkm + 1.0;
            }
            pinvsq = 1.0 / posq;

            tsi = 1.0 / (ao - sfour);
            satrec.nevalues.eta = ao * satrec.ecco * tsi;
            etasq = satrec.nevalues.eta * satrec.nevalues.eta;
            eeta = satrec.ecco * satrec.nevalues.eta;
            psisq = Math.abs(1.0 - etasq);
            coef = qzms24 * Math.pow(tsi, 4.0);
            coef1 = coef / Math.pow(psisq, 3.5);
            cc2 = coef1
                    * satrec.no
                    * (ao * (1.0 + 1.5 * etasq + eeta * (4.0 + etasq)) + 0.375
                            * j2 * tsi / psisq * satrec.nevalues.con41
                            * (8.0 + 3.0 * etasq * (8.0 + etasq)));
            satrec.nevalues.cc1 = satrec.bstar * cc2;
            cc3 = 0.0;
            if (satrec.ecco > 1.0e-4)
                cc3 = -2.0 * coef * tsi * j3oj2 * satrec.no * sinio
                        / satrec.ecco;
            satrec.nevalues.x1mth2 = 1.0 - cosio2;
            satrec.nevalues.cc4 = 2.0
                    * satrec.no
                    * coef1
                    * ao
                    * omeosq
                    * (satrec.nevalues.eta * (2.0 + 0.5 * etasq) + satrec.ecco
                            * (0.5 + 2.0 * etasq) - j2
                            * tsi
                            / (ao * psisq)
                            * (-3.0
                                    * satrec.nevalues.con41
                                    * (1.0 - 2.0 * eeta + etasq
                                            * (1.5 - 0.5 * eeta)) + 0.75
                                    * satrec.nevalues.x1mth2
                                    * (2.0 * etasq - eeta * (1.0 + etasq))
                                    * Math.cos(2.0 * satrec.argpo)));
            satrec.nevalues.cc5 = 2.0 * coef1 * ao * omeosq
                    * (1.0 + 2.75 * (etasq + eeta) + eeta * etasq);
            cosio4 = cosio2 * cosio2;
            temp1 = 1.5 * j2 * pinvsq * satrec.no;
            temp2 = 0.5 * temp1 * j2 * pinvsq;
            temp3 = -0.46875 * j4 * pinvsq * pinvsq * satrec.no;
            satrec.nevalues.mdot = satrec.no + 0.5 * temp1 * rteosq
                    * satrec.nevalues.con41 + 0.0625 * temp2 * rteosq
                    * (13.0 - 78.0 * cosio2 + 137.0 * cosio4);
            satrec.nevalues.argpdot = -0.5 * temp1 * con42 + 0.0625 * temp2
                    * (7.0 - 114.0 * cosio2 + 395.0 * cosio4) + temp3
                    * (3.0 - 36.0 * cosio2 + 49.0 * cosio4);
            xhdot1 = -temp1 * cosio;
            satrec.nevalues.omegadot = xhdot1
                    + (0.5 * temp2 * (4.0 - 19.0 * cosio2) + 2.0 * temp3
                            * (3.0 - 7.0 * cosio2)) * cosio;
            xpidot = satrec.nevalues.argpdot + satrec.nevalues.omegadot;
            satrec.nevalues.omgcof = satrec.bstar * cc3
                    * Math.cos(satrec.argpo);
            satrec.nevalues.xmcof = 0.0;
            if (satrec.ecco > 1.0e-4)
                satrec.nevalues.xmcof = -x2o3 * coef * satrec.bstar / eeta;
            satrec.nevalues.omegacf = 3.5 * omeosq * xhdot1
                    * satrec.nevalues.cc1;
            satrec.nevalues.t2cof = 1.5 * satrec.nevalues.cc1;
            satrec.nevalues.xlcof = -0.25 * j3oj2 * sinio * (3.0 + 5.0 * cosio)
                    / (1.0 + cosio);
            satrec.nevalues.aycof = -0.5 * j3oj2 * sinio;
            satrec.nevalues.delmo = Math.pow((1.0 + satrec.nevalues.eta
                    * Math.cos(satrec.mo)), 3);
            satrec.nevalues.sinmao = Math.sin(satrec.mo);
            satrec.nevalues.x7thm1 = 7.0 * cosio2 - 1.0;

            satrec.init = 0;

            /* --------------- deep space initialization ------------- */
            if ((2 * pi / satrec.no) >= 225.0) {
                satrec.nevalues.method = 2;
                satrec.nevalues.isimp = 1;
                tc = 0.0;
                inclm = satrec.inclo;

                dscom(epoch, satrec.ecco, satrec.argpo, tc, satrec.inclo,
                        satrec.omegao, satrec.no);

                satrec.mp = satrec.mo; // tmp
                satrec.argpp = satrec.argpo;
                satrec.ep = satrec.ecco;
                satrec.omegap = satrec.omegao;
                satrec.xincp = satrec.inclo;

                dpper(satrec.dsvalues.e3, satrec.dsvalues.ee2,
                        satrec.dsvalues.peo, satrec.dsvalues.pgho,
                        satrec.dsvalues.pho, satrec.dsvalues.pinco,
                        satrec.dsvalues.plo, satrec.dsvalues.se2,
                        satrec.dsvalues.se3, satrec.dsvalues.sgh2,
                        satrec.dsvalues.sgh3, satrec.dsvalues.sgh4,
                        satrec.dsvalues.sh2, satrec.dsvalues.sh3,
                        satrec.dsvalues.si2, satrec.dsvalues.si3,
                        satrec.dsvalues.sl2, satrec.dsvalues.sl3,
                        satrec.dsvalues.sl4, satrec.nevalues.t,
                        satrec.dsvalues.xgh2, satrec.dsvalues.xgh3,
                        satrec.dsvalues.xgh4, satrec.dsvalues.xh2,
                        satrec.dsvalues.xh3, satrec.dsvalues.xi2,
                        satrec.dsvalues.xi3, satrec.dsvalues.xl2,
                        satrec.dsvalues.xl3, satrec.dsvalues.xl4,
                        satrec.dsvalues.zmol, satrec.dsvalues.zmos, 1);

                satrec.mo = satrec.mp; // tmp
                satrec.argpo = satrec.argpp;
                satrec.ecco = satrec.ep;
                satrec.omegao = satrec.omegap;
                satrec.inclo = satrec.xincp;

                argpm = 0.0;
                omegam = 0.0;
                mm = 0.0;

                dsinit(cosim, emsq, satrec.argpo, s1, s2, s3, s4, s5, sinim,
                        satrec.nevalues.t, tc, satrec.dsvalues.gsto, satrec.mo,
                        satrec.nevalues.mdot, satrec.no, satrec.omegao,
                        satrec.nevalues.omegadot, xpidot, z1, z3, z11, z13,
                        z21, z23, z31, z33, satrec.ecco, eccsq);
            }

            /* ----------- set variables if not deep space ----------- */
            if (satrec.nevalues.isimp != 1) {
                cc1sq = satrec.nevalues.cc1 * satrec.nevalues.cc1;
                satrec.nevalues.d2 = 4.0 * ao * tsi * cc1sq;
                temp = satrec.nevalues.d2 * tsi * satrec.nevalues.cc1 / 3.0;
                satrec.nevalues.d3 = (17.0 * ao + sfour) * temp;
                satrec.nevalues.d4 = 0.5 * temp * ao * tsi
                        * (221.0 * ao + 31.0 * sfour) * satrec.nevalues.cc1;
                satrec.nevalues.t3cof = satrec.nevalues.d2 + 2.0 * cc1sq;
                satrec.nevalues.t4cof = 0.25 * (3.0 * satrec.nevalues.d3 + satrec.nevalues.cc1
                        * (12.0 * satrec.nevalues.d2 + 10.0 * cc1sq));
                satrec.nevalues.t5cof = 0.2 * (3.0 * satrec.nevalues.d4 + 12.0
                        * satrec.nevalues.cc1 * satrec.nevalues.d3 + 6.0
                        * satrec.nevalues.d2 * satrec.nevalues.d2 + 15.0
                        * cc1sq * (2.0 * satrec.nevalues.d2 + cc1sq));
            }
        }

        if (help == 'y')
        {
            System.out.printf("%7s\n",
                    " ------------------after sgp4init :-------------");
            System.out.printf("    inputs  : \n");
            System.out.printf(
                    "%7s%15d%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "satn", satn, "yr", year, "bstar", satrec.bstar, "ecco",
                    satrec.ecco, "epoch ", epoch, "argpo", satrec.argpo);
            System.out.printf("%7s%15.9f%7s%15.9f\n", "inclo", satrec.inclo,
                    "mo", satrec.mo);
            System.out.printf(" in and out variables \n");
            System.out.printf("%7s%15.9f\n", "no", satrec.no);
            System.out.printf("    outputs  :\n");
            System.out.printf("%7s%15d%7s%15d%7s%15d%7s%15.9f\n", "init",
                    satrec.init, "isimp", satrec.nevalues.isimp, "method",
                    satrec.nevalues.method, "aycof", satrec.nevalues.aycof);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "con41",
                    satrec.nevalues.con41, "cc1", satrec.nevalues.cc1, "cc4",
                    satrec.nevalues.cc4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "cc5",
                    satrec.nevalues.cc5, "d2", satrec.nevalues.d2, "d3",
                    satrec.nevalues.d3, "d4", satrec.nevalues.d4, "delmo",
                    satrec.nevalues.delmo);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "eta",
                    satrec.nevalues.eta, "argpdot", satrec.nevalues.argpdot,
                    "omgcof", satrec.nevalues.omgcof);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f\n", "sinmao",
                    satrec.nevalues.sinmao, "t2cof", satrec.nevalues.t2cof,
                    "t3cof", satrec.nevalues.t3cof);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "t4cof", satrec.nevalues.t4cof, "t5cof",
                    satrec.nevalues.t5cof, "gsto", satrec.dsvalues.gsto,
                    "x1mth2", satrec.nevalues.x1mth2, "x7thm1",
                    satrec.nevalues.x7thm1, "xlcof", satrec.nevalues.xlcof);
            System.out.printf("%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xmcof", satrec.nevalues.xmcof, "mdot",
                    satrec.nevalues.mdot, "omegacf", satrec.nevalues.omegacf,
                    "omegadt", satrec.nevalues.omegadot);
            System.out
                    .printf("   in and outputs from deep space satellites :\n");
            System.out.printf("%7s%15.9f%7s%15.9f\n", "t", satrec.nevalues.t,
                    "omegao", satrec.omegao);
            System.out.printf("%7s%15d%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "irez", satrec.dsvalues.irez, "atime",
                    satrec.dsvalues.atime, "d2201", satrec.dsvalues.d2201,
                    "d2211", satrec.dsvalues.d2211, "d3210",
                    satrec.dsvalues.d3210);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "d3222", satrec.dsvalues.d3222, "d4410",
                    satrec.dsvalues.d4410, "d4422", satrec.dsvalues.d4422,
                    "d5220", satrec.dsvalues.d5220, "d5232",
                    satrec.dsvalues.d5232, "d5421", satrec.dsvalues.d5421);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "d5433",
                    satrec.dsvalues.d5433, "dedt", satrec.dsvalues.dedt,
                    "del1", satrec.dsvalues.del1, "del2", satrec.dsvalues.del2,
                    "del3", satrec.dsvalues.del3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "didt",
                    satrec.dsvalues.didt, "dmdt", satrec.dsvalues.dmdt,
                    "dnodt", satrec.dsvalues.dnodt, "domdt",
                    satrec.dsvalues.domdt, "e3", satrec.dsvalues.e3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "ee2", satrec.dsvalues.ee2, "peo", satrec.dsvalues.peo,
                    "pgho", satrec.dsvalues.pgho, "pho", satrec.dsvalues.pho,
                    "pinco", satrec.dsvalues.pinco, "plo", satrec.dsvalues.plo);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "se2", satrec.dsvalues.se2, "se3", satrec.dsvalues.se3,
                    "sgh2", satrec.dsvalues.sgh2, "sgh3", satrec.dsvalues.sgh3,
                    "sgh4", satrec.dsvalues.sgh4, "sh2", satrec.dsvalues.sh2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "sh3", satrec.dsvalues.sh3, "si2", satrec.dsvalues.si2,
                    "si3", satrec.dsvalues.si3, "sl2", satrec.dsvalues.sl2,
                    "sl3", satrec.dsvalues.sl3, "sl4", satrec.dsvalues.sl4);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "xfact",
                    satrec.dsvalues.xfact, "xgh2", satrec.dsvalues.xgh2,
                    "xgh3", satrec.dsvalues.xgh3, "xgh4", satrec.dsvalues.xgh4,
                    "xh2", satrec.dsvalues.xh2);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n", "xh3",
                    satrec.dsvalues.xh3, "xi2", satrec.dsvalues.xi2, "xi3",
                    satrec.dsvalues.xi3, "xl2", satrec.dsvalues.xl2, "xl3",
                    satrec.dsvalues.xl3);
            System.out.printf(
                    "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                    "xl4", satrec.dsvalues.xl4, "xli", satrec.dsvalues.xli,
                    "xlamo", satrec.dsvalues.xlamo, "xni", satrec.dsvalues.xni,
                    "zmol", satrec.dsvalues.zmol, "zmos", satrec.dsvalues.zmos);
        }
    }

    /**
     * Initialize the SGP4 data and return the position at epoch
     * 
     * @param SatElset satElset
     * @return Sgp4Data
     */
    public Sgp4Data twoline2rv(SatElset satElset) throws ObjectDecayed {

        double tumin = 13.44654985511;

        double e1 = 0.0;
        double nuo = 0.0;
        int year = 0;

        double xpdotp = 229.1831180523293;

        satrec.error = 0;

        /** ***************** */
        /* do first card */
        /** ***************** */
        satrec.satnum = satElset.getSatID();

        satrec.epochyr = satElset.getEpochYr();
        satrec.epochdays = satElset.getEpochDay();
        satrec.ndot = satElset.getNdot();

        satrec.nddot = satElset.getNdotdot();

        satrec.bstar = satElset.getBstar();

        /** ***************** */
        /* do second card */
        /** ***************** */

        satrec.inclo = satElset.getInclinationDeg();

        satrec.omegao = satElset.getRightAscensionDeg();
        satrec.ecco = satElset.getEccentricity();
        satrec.argpo = satElset.getArgPerigeeDeg();

        satrec.mo = satElset.getMeanAnomalyDeg();
        satrec.no = satElset.getMeanMotion();

        // * ---- convert to sgp4 units ----
        satrec.no = satrec.no / xpdotp; // * rad/min

        satrec.a = Math.pow(satrec.no * tumin, (-2.0 / 3.0));
        satrec.ndot = satrec.ndot / (xpdotp * 1440.0); // * ? * minperday
        satrec.nddot = satrec.nddot / (xpdotp * 1440.0 * 1440);

        // * ---- find standard orbital elements ----
        satrec.inclo = satrec.inclo / rad;
        satrec.omegao = satrec.omegao / rad;
        satrec.argpo = satrec.argpo / rad;
        satrec.mo = satrec.mo / rad;

        // FIXME e1 and nuo not returned, should they be?
        newtonm(satrec.ecco, satrec.mo, e1, nuo);

        satrec.alta = satrec.a * (1.0 + satrec.ecco * satrec.ecco) - 1.0;
        satrec.altp = satrec.a * (1.0 - satrec.ecco * satrec.ecco) - 1.0;

        // ----------------------------------------------------------------
        // find sgp4epoch time of element set
        // remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
        // and minutes from the epoch (time)
        // --------------------------------------------------------------

        // * ------------ temp fix for years from 1950-2049 ---------------
        // ------- double fix will occur when year is 4-digit in 2le ------
        if (satrec.epochyr < 50)
            year = satrec.epochyr + 2000;
        else
            year = satrec.epochyr + 1900;

        // Epoch time
        satrec.eptime = (year - 1950) * 365 + (year - 1949) / 4
                + satrec.epochdays;

        int jDays = (int) Math.floor(satrec.epochdays);
        double remainder = (satrec.epochdays - jDays);
        int hrs = (int) Math.floor(remainder * 24.0);
        remainder = remainder * 24.0 - (double) hrs;
        int min = (int) Math.floor(remainder * 60.0);
        remainder = remainder * 60.0 - (double) min;
        double dsecs = remainder * 60.0;

        // Use the calendar function to get the month for the
        // modified Julian day conversion routine
        GregorianCalendar calendar = new GregorianCalendar(tz);
        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.DAY_OF_YEAR, jDays);
        calendar.set(Calendar.HOUR_OF_DAY, hrs);
        calendar.set(Calendar.MINUTE, min);

        // for consistency with C code, use dsecs which does not round to
        // milliseconds
        satrec.mjdsatepoch = julianday(year, calendar.get(Calendar.MONTH) + 1,
                calendar.get(Calendar.DAY_OF_MONTH), hrs, min, dsecs);

        satrec.mjdsatepoch = satrec.mjdsatepoch - 2400000.5;

        // ------------- initialize the orbit at sgp4epoch --------------
        // ---------- init and the time must be set as shown! -----------
        satrec.init = 1;
        satrec.nevalues.t = 0.0;

        sgp4init(satrec.satnum, year, satrec.mjdsatepoch - 33281.0);

        return sgp4();

    }

    /**
     * Initialize the SGP4 data and return the position at epoch
     * 
     * @param String card1
     * @param String card2
     * @return Sgp4Data
     */
    public Sgp4Data twoline2rv(String card1, String card2)
            throws SatElsetException, ObjectDecayed {

        double tumin = 13.44654985511;

        double e1 = 0.0;
        double nuo = 0.0;
        int year = 0;

        double xpdotp = 229.1831180523293;

        satrec.error = 0;

        // Read the two line element set into the SatElset to
        // make it easier to extract the orbital parameters
        SatElset satElset = new SatElset(card1, card2);

        /** ***************** */
        /* do first card */
        /** ***************** */
        satrec.satnum = satElset.getSatID();

        satrec.epochyr = satElset.getEpochYr();
        satrec.epochdays = satElset.getEpochDay();
        satrec.ndot = satElset.getNdot();

        satrec.nddot = satElset.getNdotdot();

        satrec.bstar = satElset.getBstar();

        /** ***************** */
        /* do second card */
        /** ***************** */

        satrec.inclo = satElset.getInclinationDeg();

        satrec.omegao = satElset.getRightAscensionDeg();
        satrec.ecco = satElset.getEccentricity();
        satrec.argpo = satElset.getArgPerigeeDeg();

        satrec.mo = satElset.getMeanAnomalyDeg();
        satrec.no = satElset.getMeanMotion();

        // * ---- convert to sgp4 units ----
        satrec.no = satrec.no / xpdotp; // * rad/min

        satrec.a = Math.pow(satrec.no * tumin, (-2.0 / 3.0));
        satrec.ndot = satrec.ndot / (xpdotp * 1440.0); // * ? * minperday
        satrec.nddot = satrec.nddot / (xpdotp * 1440.0 * 1440);

        // * ---- find standard orbital elements ----
        satrec.inclo = satrec.inclo / rad;
        satrec.omegao = satrec.omegao / rad;
        satrec.argpo = satrec.argpo / rad;
        satrec.mo = satrec.mo / rad;

        // FIXME e1 and nuo not returned, should they be?
        newtonm(satrec.ecco, satrec.mo, e1, nuo);

        satrec.bstar = satrec.bstar;

        satrec.alta = satrec.a * (1.0 + satrec.ecco * satrec.ecco) - 1.0;
        satrec.altp = satrec.a * (1.0 - satrec.ecco * satrec.ecco) - 1.0;

        // ----------------------------------------------------------------
        // find sgp4epoch time of element set
        // remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
        // and minutes from the epoch (time)
        // --------------------------------------------------------------

        // * ------------ temp fix for years from 1950-2049 ---------------
        // ------- double fix will occur when year is 4-digit in 2le ------
        if (satrec.epochyr < 50)
            year = satrec.epochyr + 2000;
        else
            year = satrec.epochyr + 1900;

        // Epoch time
        satrec.eptime = (year - 1950) * 365 + (year - 1949) / 4
                + satrec.epochdays;

        // SystemTime epochTime = new SystemTime(satrec.epochdays, year);

        // System.out.println("Epoch: " + epochTime.getYr() + " "
        // + epochTime.getMonth() + " " + epochTime.getDayOfMonth() + " "
        // + epochTime.getHrs() + " " + epochTime.getMin() + " "
        // + (epochTime.getSec() + epochTime.getMillisec() / 1000.0));

        int jDays = (int) Math.floor(satrec.epochdays);
        double remainder = (satrec.epochdays - jDays);
        int hrs = (int) Math.floor(remainder * 24.0);
        remainder = remainder * 24.0 - (double) hrs;
        int min = (int) Math.floor(remainder * 60.0);
        remainder = remainder * 60.0 - (double) min;
        double dsecs = remainder * 60.0;

        // get the modified julian date
        // satrec.mjdsatepoch = julianday(epochTime.getYr(),
        // epochTime.getMonth()+1,
        // epochTime.getDayOfMonth(), epochTime.getHrs(), epochTime
        // .getMin(), epochTime.getSec() + epochTime.getMillisec()
        // / 1000.0);

        // Use the calendar function to get the month for the
        // modified Julian day conversion routine
        GregorianCalendar calendar = new GregorianCalendar(tz);
        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.DAY_OF_YEAR, jDays);
        calendar.set(Calendar.HOUR_OF_DAY, hrs);
        calendar.set(Calendar.MINUTE, min);

        // for consistency with C code, use dsecs which does not round to
        // milliseconds
        satrec.mjdsatepoch = julianday(year, calendar.get(Calendar.MONTH) + 1,
                calendar.get(Calendar.DAY_OF_MONTH), hrs, min, dsecs);

        satrec.mjdsatepoch = satrec.mjdsatepoch - 2400000.5;

        // ------------- initialize the orbit at sgp4epoch --------------
        // ---------- init and the time must be set as shown! -----------
        satrec.init = 1;
        satrec.nevalues.t = 0.0;

        // sgp4init(satrec.satnum, year, satrec.mjdsatepoch - 33281.0,
        // satrec.bstar, satrec.ecco, satrec.argpo, satrec.inclo,
        // satrec.mo, satrec.no, satrec.omegao, satrec.init,
        // satrec.nevalues, satrec.dsvalues);
        sgp4init(satrec.satnum, year, satrec.mjdsatepoch - 33281.0);

        return sgp4();

    }

}
