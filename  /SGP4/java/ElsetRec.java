package sgp4v;

/**
 * Data Class for SGP4
 * 
 * @author Joe Coughlin
 *
 */
public class ElsetRec {
    protected int satnum;

    protected int epochyr;
    protected int init;
    protected int epochtynumrev;

    protected int error;

    protected NearEarthType nevalues = new NearEarthType();

    protected DeepSpaceType dsvalues = new DeepSpaceType();

    protected double a;
    protected double altp;
    protected double alta;
    protected double epochdays;
    protected double mjdsatepoch;
    protected double nddot;
    protected double ndot;
    protected double bstar;
    protected double rcse;
    protected double inclo;
    protected double omegao;
    protected double ecco;
    protected double argpo;
    protected double mo;
    protected double no;
    protected double eptime;
    protected double srtime;
    protected double sptime;
    protected double deltamin;

    protected double ep;
    protected double xincp;
    protected double omegap;
    protected double argpp;
    protected double mp;
    
    protected int size = 3;

    protected double[] r = new double[size];

    protected double[] v = new double[size];

    /**
     * constructor
     * @return
     */
    public ElsetRec() {
    }
}
