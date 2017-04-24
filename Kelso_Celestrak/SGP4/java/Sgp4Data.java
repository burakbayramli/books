package sgp4v;

import javax.vecmath.Vector3d;


/**
 * Class to store data associated with the calculation of satellite positions
 * 
 * <b>Technical Description: </b> <br>
 * This is a data class to return values from the SGP4 algorithms.
 * <p>
 * <p>
 * <b>References: </b> <br>
 * 
 * @see astro.sgp4.Sgp4
 *      <p>
 * 
 * @author Joe Coughlin
 * @version 1.0 4 Apr 2005
 *          <p>
 *          <b>Modifications: </b> <br>
 *          4 Apr 2005 Joe Coughlin Initial version <br>
 */
public class Sgp4Data {

    private int satNumber = -1;
    private double azimuth = 999.0;
    private double elevation = 99.0;
    private double range = 999.0;
    private double rangeRate = -999.0;
    private Vector3d posn = new Vector3d();
    private Vector3d vel = new Vector3d();
    private double radiansToDegrees = 180.0/Math.PI;
    
    /**
     * Constructor
     * @param satNum
     */
    public Sgp4Data(int satNum){
        satNumber = satNum;
    }
    /**
     * Get the Azimuth (radians)
     * @return double
     */
    public double getAzimuth() {
        return azimuth;
    }
    /**
     * Get the Azimuth (degrees)
     * @return double
     */
    public double getAzimuthDegrees() {
        return azimuth*radiansToDegrees;
    }
    /**
     * Set the azimuth (radians)
     * @param azimuth
     */
    public void setAzimuth(double azimuth) {
        this.azimuth = azimuth;
    }
    /**
     * Get the Elevation (radians)
     * @return
     */
    public double getElevation() {
        return elevation;
    }
    /**
     * Get the Elevation (degrees)
     * @return
     */
    public double getElevationDegrees() {
        return elevation*radiansToDegrees;
    }
    /**
     * Set the Elevation (radians)
     * @param double elevation
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }
    /**
     * Get the Position
     * @return Vector3d
     */
    public Vector3d getPosn() {
        return posn;
    }
    /**
     * @param posn The posn to set.
     */
    public void setPosn(Vector3d posn) {
        this.posn = posn;
    }
    /**
     * Get the range (meters)
     * @return double
     */
    public double getRange() {
        return range;
    }
    /**
     * Get the range rate (meters/sec)
     * @return double
     */
    public double getRangeRate() {
        return rangeRate;
    }
    /**
     * @param range The range to set.
     */
    public void setRange(double range) {
        this.range = range;
    }
    /**
     * @return Returns the satNumber.
     */
    public int getSatNumber() {
        return satNumber;
    }
    /**
     * @param satNumber The satNumber to set.
     */
    public void setSatNumber(int satNumber) {
        this.satNumber = satNumber;
    }
    /**
     * @return Returns the x.
     */
    public double getX() {
        return posn.x;
    }
    /**
     * @param x The x to set.
     */
    public void setX(double x) {
        posn.x = x;
    }
    /**
     * @return Returns the y.
     */
    public double getY() {
        return posn.y;
    }
    /**
     * @param y The y to set.
     */
    public void setY(double y) {
        posn.y = y;
    }
    /**
     * @return Returns the z.
     */
    public double getZ() {
        return posn.z;
    }
    /**
     * @param z The z to set.
     */
    public void setZ(double z) {
        posn.z = z;
    }
    /**
     * @return Returns the vel.
     */
    public Vector3d getVel() {
        return vel;
    }
    /**
     * @param vel The vel to set.
     */
    public void setVel(Vector3d vel) {
        this.vel = vel;
    }
    /**
     * @return Returns the xdot.
     */
    public double getXdot() {
        return vel.x;
    }
    /**
     * @param xdot The xdot to set.
     */
    public void setXdot(double xdot) {
        vel.x = xdot;
    }
    /**
     * @return Returns the ydot.
     */
    public double getYdot() {
        return vel.y;
    }
    /**
     * @param ydot The ydot to set.
     */
    public void setYdot(double ydot) {
        vel.y = ydot;
    }
    /**
     * @return Returns the zdot.
     */
    public double getZdot() {
        return vel.z;
    }
    /**
     * @param zdot The zdot to set.
     */
    public void setZdot(double zdot) {
        vel.z = zdot;
    }
}
