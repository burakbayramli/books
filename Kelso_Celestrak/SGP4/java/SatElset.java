package sgp4v;

import java.io.Serializable;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Calendar;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.TimeZone;

/**
 * This class encapsulates the standard 2-line orbital element set for a
 * satellite.
 * 
 * <b>Technical Description: </b> <br>
 * This class holds the satellite element set data. Checks are performed upon
 * satellite initialization to verify that the input data in the the standard
 * two-line element set format
 * <p>
 * <p>
 * <b>References: </b> <br>
 * <p>
 * 
 * @author Joe Coughlin
 * @version 1.0 27 May 2004
 *          <p>
 *          <b>Modifications: </b> <br>
 *          27 May 2004 Joe Coughlin Initial version <br>
 *          17 Oct 2005 J Coughlin Increase day range to 367 to handle folder
 *          elsets <br>
 *          02 Jun 2007 Egemen Imre Made small fixes regarding locale handling 
 *          and checksum of card 1 in getCard1()
 */

public class SatElset implements Serializable
{
    /** Serial version UID. */
    private static final long serialVersionUID = 0L;
    
    /** default card condition */
    private static final String BLANKCARD = "                                                                     ";

    /** Argument of Perigee */
    private double argPerigee;

    /** SGP4 drag term */
    private double bstar;

    private DecimalFormat df1 = new DecimalFormat("0");
      
    /** FIX: added US locale-specific format (decimal symbol causing problems at different locales)  */
    private DecimalFormat df2dot8 = new DecimalFormat("00.00000000", new DecimalFormatSymbols(Locale.US));
//  private DecimalFormat df2dot8 = new DecimalFormat("00.00000000");

    /** FIX: added US locale-specific format (decimal symbol causing problems at different locales)  */
    private DecimalFormat df3dot4 = new DecimalFormat("000.0000", new DecimalFormatSymbols(Locale.US));
//  private DecimalFormat df3dot4 = new DecimalFormat("000.0000");

    /** FIX: added US locale-specific format (decimal symbol causing problems at different locales)  */
    private DecimalFormat df3dot8 = new DecimalFormat("000.00000000", new DecimalFormatSymbols(Locale.US));
//  private DecimalFormat df3dot8 = new DecimalFormat("000.00000000");

    private DecimalFormat df4 = new DecimalFormat("0000");

    private DecimalFormat df5 = new DecimalFormat("00000");

    /** FIX: added US locale-specific format (decimal symbol causing problems at different locales)  */
    private DecimalFormat dfdot7 = new DecimalFormat(".0000000", new DecimalFormatSymbols(Locale.US));
//  private DecimalFormat dfdot7 = new DecimalFormat(".0000000");

    /** FIX: added US locale-specific format (decimal symbol causing problems at different locales)  */
    private DecimalFormat dfdot8 = new DecimalFormat(".00000000", new DecimalFormatSymbols(Locale.US));
//  private DecimalFormat dfdot8 = new DecimalFormat(".00000000");

    /** orbit shape [0.0, < 1.0] */
    private double eccentricity;

    /** number of prior elset updates [0,9999] */
    private int elsetNum;

    /** orbit model to use [0 = SGP(nd/2,ndd/6), 2 = SGP4(Bstar)] */
    private int ephemerisType;

    /** epoch day [0.0,366.0] */
    private double epochDay;

    /** epoch year [0,99] */
    private int epochYr;

    /** orbital inclination (radians) [0.0,180.0] */
    private double inclination;

    /** International designator */
    private String intDesig;

    /** is the data on the 2 lines valid? */
    private boolean isValid;

    /** Mean Anomaly (radians) */
    private double meanAnomaly;

    /** revolutions per day */
    private double meanMotion;

    /** optional satellite name */
    private String name;

    private double nDot;

    private double nDotDot;

    /** revolutions since launch (deployment...) */
    private int revNum;

    /** right ascension of the ascending node (radians) [0.0,360.0] */
    private double rightAscension;

    /** Classification: U, C, S, or T */
    private String satClass;

    /** SSC# [1,99999] */
    private int satID;

    private static final double TODEGREES = 180.0 / Math.PI;

    private static final double TORADIANS = Math.PI / 180.0;

    private static final double TWOPI = Math.PI * 2.0;

    /**
     * SatElset default constructor.
     */
    public SatElset() {
        name = " ";
        isValid = false;
        
    }

    /**
     * Copy Constructor
     * 
     * @param elset the SatElset instance to copy.
     */
    public SatElset(SatElset elset) {
        name = new String(elset.getName());
        if (name == null) {
            name = " ";
        }
        // copy data
        this.argPerigee = elset.argPerigee;
        this.bstar = elset.bstar;
        this.eccentricity = elset.eccentricity;
        this.elsetNum = elset.elsetNum;
        this.ephemerisType = elset.ephemerisType;
        this.epochDay = elset.epochDay;
        this.epochYr = elset.epochYr;
        this.inclination = elset.inclination;
        this.intDesig= elset.intDesig;
        this.meanAnomaly = elset.meanAnomaly;
        this.meanMotion = elset.meanMotion;
        this.nDot = elset.nDot;
        this.nDotDot = elset.nDotDot;
        this.revNum = elset.revNum;
        this.rightAscension = elset.rightAscension;
        this.satClass = elset.satClass;
        this.satID = elset.satID;
    }

    /**
     * This constructor takes the two lines of the standard 2-line elset format
     * as inputs and calls the constructor with a blank name.
     * 
     * @param card1 the String containing line 1 of the elset
     * @param card2 the String containing line 2 of the elset
     */
    public SatElset(String card1, String card2) throws SatElsetException {

        this(" ", card1, card2);
        
    }

    /**
     * This constructor takes the sat name and the two lines of the standard
     * 2-line elset format as inputs. The ends of the cards are checked for
     * additional columns containing average RCS and TOES values. Only the
     * standard columns are preserved in card1 and card2. Then lengths of card1
     * and card2 are truncated or extended to 69 characters
     * 
     * @param name the String containing the satellite name
     * @param card1 the String containing line 1 of the elset (plus possible
     *        avgRCS value)
     * @param card2 the String containing line 2 of the elset (plus possible
     *        TOES value)
     */
    public SatElset(String name, String card1, String card2)
            throws SatElsetException {

    	
        if (name == null) {
            this.name = " ";
        } else {
            this.name = new String(name);
        }

        int slen = card1.length();
        if (slen <= 69) {
            card1 += BLANKCARD.substring(0, 69 - slen);
        }
        String line1 = card1.substring(0, Math.min(card1.length(), 69));

        slen = card2.length();
        if (slen <= 69) {
            card2 += BLANKCARD.substring(0, 69 - slen);
        }
        String line2 = card2.substring(0, Math.min(card2.length(), 69));

        // Check if the data are valid and if the first 5 characters in
        // both elset lines are the same
        try {
            isValid = card1IsValid(line1) && card2IsValid(line2)
                    && line1.regionMatches(2, line2, 2, 5);

        } catch (Exception exc) {
            String errString = exc.toString() + "\n"
                    + "SatElset constructor Invalid elset:\n" + "Card1: ["
                    + line1 + "]\n" + "Card2: [" + line2 + "]";
            throw new SatElsetException(errString);
        }
    }

    /**
     * This method validates the data fields and checksum, if available, on the
     * first elset card.
     * 
     * @return boolean = true if card 1 contains valid data
     */
    public boolean card1IsValid(String card) throws SatElsetException,
            ValueOutOfRangeException {

        if (card == null) {
            String errString = "SatElset.card1IsValid Card 1 is null";
            throw new SatElsetException(errString);
        }

        if (card.length() < 68) {
            String errString = "SatElset.card1IsValid Card 1 length < 68 length = "
                    + card.length();
            throw new SatElsetException(errString);
        }

        if (!"1".equals(card.substring(0, 1))) {
            String errString = "SatElset.card1IsValid Card 1 not 1: [" + card
                    + "]";
            throw new SatElsetException(errString);
        }

        try {
            satID = Integer.parseInt(card.substring(2, 7).replace(' ','0'));
            if (satID < 1 || satID > 99999) {
                String errString = "SatElset.card1IsValid Card 1 satID number out of range *"
                        + card.substring(2, 7) + "*";
                throw new ValueOutOfRangeException(errString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card1IsValid Card 1 satID number format exception *"
                    + card.substring(2, 7) + "*";
            String errString2 = "SatElset.card1IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        satClass = card.substring(7, 8);
        if (!"U".equals(satClass) && !"C".equals(satClass)
                && !"S".equals(satClass) && !"T".equals(satClass)) {
            String errString = "SatElset.card1IsValid Card 1 classification error: ["
                    + card.charAt(7) + "]";
            throw new SatElsetException(errString);
        }

        // International designator
        intDesig = card.substring(9, 17);

        try {
            epochYr = Integer.parseInt(card.substring(18, 20));
            if (epochYr < 0 || epochYr > 99) {
                String errString = "SatElset.card1IsValid Card 1 epochYr out of range: ["
                        + card.substring(18, 20) + "]";
                throw new ValueOutOfRangeException(errString);
            }

        } catch (NumberFormatException ex) {
            String errString = "SatElset.card1IsValid Card 1 epochYr number format exception: ["
                    + card.substring(18, 20) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card1IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            epochDay = Double.valueOf(card.substring(20, 32)).doubleValue();
            if (epochDay < 0.0 || epochDay > 367.0) {
                String errString = "SatElset.card1IsValid Card 1 epochDay out of range: ["
                        + card.substring(20, 32) + "]";
                throw new ValueOutOfRangeException(errString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card1IsValid Card 1 epochDay number format exception: ["
                    + card.substring(20, 32) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card1IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }
        
        /*
         * Now that we have the epochYr and epochDay, create a UTCTime
         * with the epoch time of this sat, and store it as setUTCTime.
         * (JRo steals code from Dominic B.)
         */
        
        final int secPerMin = 60;
        final int secPerHour = secPerMin * 60;
        final int secPerDay = secPerHour * 24;
        
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        int day = (int)epochDay;

        double tod = (epochDay-day)*secPerDay;

        int hour = (int)(tod/(double)secPerHour);
        int minute = (int)((tod-(double)hour*secPerHour)/secPerMin);
        int second = (int)((tod-(double)(hour*secPerHour+minute*secPerMin)));
        int milSec = (int)((tod - (int)tod)*1000.0);

        cal.set(Calendar.YEAR,((epochYr+50) % 100) + 1950);
        cal.set(Calendar.DAY_OF_YEAR,day);
        cal.set(Calendar.HOUR_OF_DAY,hour);
        cal.set(Calendar.MINUTE,minute);
        cal.set(Calendar.SECOND,second);
        cal.set(Calendar.MILLISECOND,milSec);

        // N Dot
        try {
            nDot = Double.valueOf(card.substring(33, 43));
        } catch (NumberFormatException exc) {
            String errString = "SatElset.card1IsValid Invalid Ndot for Satellite number: "
                    + satID + " [" + card.substring(33, 43) + "]";
            String errString2 = "SatElset.card1IsValid [" + exc + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            String tmpStr;
            if (card.charAt(50) == ' ') {
                tmpStr = card.substring(44, 45) + "." + card.substring(45, 50)
                        + "E+" + card.substring(51, 52);
            } else {
                tmpStr = card.substring(44, 45) + "." + card.substring(45, 50)
                        + "E" + card.substring(50, 52);
            }
            nDotDot = Double.valueOf(tmpStr).doubleValue();
        } catch (NumberFormatException exc) {
            String errString = "SatElset.card1IsValid Invalid NdotDot for Satellite number: "
                    + satID + " [" + card.substring(44, 52) + "]";
            String errString2 = "SatElset.card1IsValid [" + exc + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        // BSTAR
        try {
            String expString = card.substring(59, 61);
            if (" 0".equalsIgnoreCase(expString)) {
                expString = "00";
            }
            bstar = Double.valueOf(
                    card.substring(53, 54) + "." + card.substring(54, 59) + "E"
                            + expString).doubleValue();
        } catch (NumberFormatException exc) {
            String errString = "SatElset.card1IsValid Invalid BSTAR for Satellite number: "
                    + satID + " [" + card.substring(53, 61) + "]";
            String errString2 = "SatElset.card1IsValid [" + exc + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            if (" ".equals(card.substring(62, 63))) {
                String errString = "SatElset.card1IsValid No Ephemeris Type for Satellite number: "
                        + satID + " [" + card.substring(62, 63) + "], Set to 0";
                throw new SatElsetException(errString);
            } else {
                ephemerisType = Integer.parseInt(card.substring(62, 63));
            }
            // Allow for SGP4 (2) or SGP (0) cards
            if (ephemerisType != 0 && ephemerisType != 2) {
                String errString = "SatElset.card1IsValid Card 1 ephemerisType out of range: ["
                        + card.substring(62, 63)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card1IsValid Card 1 ephemerisType number format exception: ["
                    + card.substring(62, 63) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card1IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        StringTokenizer st = new StringTokenizer(card.substring(65, 68), " ");
        try {
            elsetNum = Integer.parseInt((String) st.nextElement());
            if (elsetNum < 0 || elsetNum > 999) {
                String errString = "SatElset.card1IsValid Card 1 elsetNum number out of range: ["
                        + card.substring(65, 68) + "]";
                throw new ValueOutOfRangeException(errString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card1IsValid Card 1 elsetNum number format exception: ["
                    + card.substring(65, 68) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card1IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        if (card.length() > 68 && !" ".equals(card.substring(68, 69))) {
            if (checkSum(card) != Integer.parseInt(card.substring(68, 69))) {
                String errorString = "SatElset.card1IsValid Check Sum error on Card 1. Expecting: "
                        + checkSum(card)
                        + " Actual value on card: "
                        + Integer.parseInt(card.substring(68, 69))
                        + " Satellite number: " + satID;
                throw new SatElsetException(errorString);
            }
        }
        return true;
    }

    /**
     * This method validates the data fields and checksum, if available, on the
     * second elset card.
     * 
     * @return boolean = true if card contains valid data for the second elset
     *         line
     */
    public boolean card2IsValid(String card) throws SatElsetException,
            ValueOutOfRangeException {

        int satID = 0; // SSC# [1,99999]

        if (card == null) {
            return false;
        }

        if (card.length() < 68) {
            return false;
        }

        if (!"2".equals(card.substring(0, 1))) {
            return false;
        }

        try {
            satID = Integer.parseInt(card.substring(2, 7).replace(' ','0'));
            if (satID < 1 || satID > 99999) {
                String errorString = "SatElset.card2IsValid Card 2 satID number out of range: ["
                        + card.substring(2, 7) + "]";
                throw new ValueOutOfRangeException(errorString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 satID number format exception: ["
                    + card.substring(2, 7) + "]";
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            double testInclination = Double.valueOf(card.substring(8, 17))
                    .doubleValue();
            if (testInclination < 0.0 || testInclination > 180.0) {
                String errorString = "SatElset.card2IsValid Card 2 inclination out of range: ["
                        + card.substring(8, 17)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errorString);
            }
            inclination = testInclination * TORADIANS;

        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 inclination number format exception: ["
                    + card.substring(8, 17) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            double rtAsc = Double.valueOf(card.substring(17, 26)).doubleValue();
            if (rtAsc < 0.0 || rtAsc > 360.0) {
                String errorString = "SatElset.card2IsValid Card 2 rightAcension out of range: ["
                        + card.substring(17, 26)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errorString);
            }
            rightAscension = rtAsc * TORADIANS;

        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 rightAcension number format exception: ["
                    + card.substring(17, 26) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            double testArgPerigee = Double.valueOf(card.substring(34, 42))
                    .doubleValue();
            if (testArgPerigee < 0.0 || testArgPerigee > 360.0) {
                String errorString = "SatElset.card2IsValid Card 2 argPerigee out of range: ["
                        + card.substring(34, 42)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errorString);
            }
            argPerigee = testArgPerigee * TORADIANS;

        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 argPerigee number format exception: ["
                    + card.substring(34, 42) + "]  Satellite number: " + satID;

            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            double testEccentricity = Double.valueOf(
                    "0." + card.substring(26, 33)).doubleValue();
            if (testEccentricity < 0.0 || testEccentricity >= 1.0) {
                String errorString = "SatElset.card2IsValid Card 2 eccentricity out of range: ["
                        + "0."
                        + card.substring(26, 33)
                        + "]  Satellite number: " + satID;
                throw new ValueOutOfRangeException(errorString);
            }
            eccentricity = testEccentricity;
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 eccentricity number format exception: ["
                    + "0."
                    + card.substring(26, 33)
                    + "]  Satellite number: "
                    + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            double testMeanAnomaly = Double.valueOf(card.substring(43, 51))
                    .doubleValue();
            if (testMeanAnomaly < 0.0 || testMeanAnomaly > 360.0) {
                String errorString = "SatElset.card2IsValid Card 2 meanAnomaly out of range: ["
                        + card.substring(43, 51)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errorString);
            }
            meanAnomaly = testMeanAnomaly * TORADIANS;
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 meanAnomaly number format exception: ["
                    + card.substring(43, 51) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        try {
            meanMotion = Double.valueOf(card.substring(52, 63)).doubleValue();
            if (meanMotion > 17.0) {
                String errorString = "SatElset.card2IsValid Card 2 meanMotion out of range: ["
                        + card.substring(52, 63)
                        + "]  Satellite number: "
                        + satID;
                throw new ValueOutOfRangeException(errorString);
            }
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 meanMotion number format exception: ["
                    + card.substring(52, 63) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        StringTokenizer st = new StringTokenizer(card.substring(63, 68), " ");
        try {
            revNum = Integer.parseInt((String) st.nextElement());
        } catch (NumberFormatException ex) {
            String errString = "SatElset.card2IsValid Card 2 revNum number format exception: ["
                    + card.substring(63, 68) + "]  Satellite number: " + satID;
            String errString2 = "SatElset.card2IsValid [" + ex + "]";
            throw new SatElsetException(errString + "\\n" + errString2);
        }

        if (revNum < 0 || revNum > 99999) {
            String errorString = "SatElset.card2IsValid Card 2 revNum out of range: ["
                    + card.substring(52, 63) + "]  Satellite number: " + satID;
            throw new ValueOutOfRangeException(errorString);
        }

        // FIXME need to determine if this is ignored or not
        if (card.length() > 68 && !" ".equals(card.substring(68, 69))) {
            if (checkSum(card) != Integer.parseInt(card.substring(68, 69))) {
                String errString = "SatElset.card2IsValid Check Sum error on Card 2. Expecting: "
                        + checkSum(card)
                        + " Actual value on card: "
                        + Integer.parseInt(card.substring(68, 69))
                        + " Satellite number: " + satID;
                throw new SatElsetException(errString);
            }
        }

        return true;
    }

    /**
     * This method computes the checksum of 1 card of a a 2-line elset. The
     * first 68 characters are scanned and "summed" to get the checksum.
     * 
     * @return the int containing the checksum [0,9]
     * @param card the String containing the card image
     */
    private int checkSum(String card) {
        int checksum = 0;

        for (int i = 0; i < 68; i++) {
            switch (card.charAt(i)) {
                case '1':
                /* falls through */
                case '-':
                    checksum++;
                    break;
                case '2':
                    checksum += 2;
                    break;
                case '3':
                    checksum += 3;
                    break;
                case '4':
                    checksum += 4;
                    break;
                case '5':
                    checksum += 5;
                    break;
                case '6':
                    checksum += 6;
                    break;
                case '7':
                    checksum += 7;
                    break;
                case '8':
                    checksum += 8;
                    break;
                case '9':
                    checksum += 9;
                    break;
                default:
                    break;
            }
        }

        return checksum % 10;
    }

    /**
     * Compares two objects for equality. Returns a boolean that indicates
     * whether this object is equivalent to the specified object. This method is
     * used when an object is stored in a hashtable.
     * 
     * @param elset the SatElset to compare with
     * @return true if these Objects are equal; false otherwise.
     * @see java.util.Hashtable
     */
    public boolean equals(SatElset elset) {
        // FIXME equality
        return false;
    }

    /**
     * This method analyzes the supplied value val and breaks it into sign,
     * mantissa, and exponent parts, each of which is constructed as an integer.
     * The mantissa is constrained to be either 00000 or be in the range
     * [10000,99999] (5 significant digits), representing the values 0.10000 to
     * 0.99999. The exponent must lie in the range [-9,9] (single digit). The
     * returned mantissa and exponent are "clamped" at the limit values 0000 and
     * 0 or 9999 and 9 if the constraints can't be achieved.
     * 
     * @return boolean true if the formatting could be done within the
     *         constraints
     * @param val the double value to format
     * @param parts the int array containing the sign, mantissa, and exponent
     */
    private boolean formatInt(double val, int[] parts) {

        double absVal = Math.abs(val);
        int exp = 0;

        // determine sign of value
        parts[0] = (val < 0.0) ? -1 : 1;

        if (absVal <= 1.0E-10) {
            // too small in magnitude to fit format constraints
            parts[1] = 0;
            parts[2] = 0;
            return false;
        }

        if (absVal >= 1.0E9) {
            // too large in magnitude to fit format constraints
            parts[1] = 99999;
            parts[2] = 9;
            return false;
        }

        exp = 0; // start with representation absVal * 10^0
        while (absVal > 1.0) { // reduce absVal and increase exponent parts[2]
            absVal /= 10.0;
            exp++;
        }
        while (absVal < 0.1) { // increase absVal and reduce exponent parts[2]
            absVal *= 10.0;
            exp--;
        }
        parts[1] = (int) (absVal * 1.0E5 + 0.5);
        // save first 5 significant digits as a integer
        parts[2] = exp;

        return true;
    }

    /**
     * This method returns the orbital argument of perigee as a double.
     * 
     * @return the double containing the argument of perigee in radians
     */
    public double getArgPerigee() {
        return argPerigee;
    }

    /**
     * This method returns the orbital argument of perigee as a double.
     * 
     * @return the double containing the argument of perigee in degrees [0,360]
     */
    public double getArgPerigeeDeg() {
        return argPerigee * TODEGREES;
    }

    /**
     * This method returns the orbital argument of perigee as a string.
     * 
     * @return the String containing the argument of perigee (NNN.NNNN) in
     *         degrees [0,360]
     */
    public String getArgPerigeeString() {
        DecimalFormat df4 = new DecimalFormat("000.0000");
        String argPerStr = df4.format(argPerigee * TODEGREES);
        return argPerStr;
    }

    /**
     * This method returns the orbital drag term as a string.
     * 
     * @return the String containing the drag term (S.NNNNNESE) in 1/earth-radii
     */
    public String getBstarString() {

        String card1 = getCard1();
        return card1.substring(53, 54) + "." + card1.substring(54, 59) + "E"
                + card1.substring(59, 61);
    }

    /**
     * This method returns the orbital drag term as a double.
     * 
     * @return the double containing the drag term (1/earth-radii)
     */
    public double getBstar() {
        return bstar;
    }

    /**
     * This method returns the first line of the elset.
     * 
     * @return the String containing card1
     */
    public String getCard1() {
    	
        String card1 = "1 00001U 00000000 70001.00000000 +.00000000 +00000+0 +00000+0 0 00009";

        // Sat ID
        String satIDstr = df5.format(satID);
        StringBuffer card1sb = new StringBuffer(card1);
        card1sb.replace(2, 7, satIDstr);

        card1sb.replace(7, 8, satClass);

        // International Designator
        int idLen = intDesig.length();
        if (idLen > 0) {
            if (idLen < 8) {
                intDesig += BLANKCARD.substring(0, 8 - idLen);
            }
            card1sb.replace(9, 17, intDesig.substring(0, 8));
        }

        DecimalFormat df = new DecimalFormat(" 00");
        int tempYr = epochYr;
        if (tempYr > 1900) {
            tempYr -= 1900;
            if (tempYr > 100) {
                tempYr -= 100;
            }
        }
        String yrStr = df.format(tempYr);
        card1sb.replace(17, 20, yrStr);

        // Epoch Day
        String dayStr = df3dot8.format(epochDay);
        card1sb.replace(20, 32, dayStr);

        // N dot
        String nDotStr = dfdot8.format(nDot);
        if (nDotStr.charAt(0) == '-') {
            card1sb.replace(33, 43, nDotStr);
        } else {
            card1sb.replace(33, 43, " " + nDotStr);
        }

        // N dot dot
        int[] parts = new int[3]; // sign, mantissa, and power of 10
        String signStr;

        formatInt(nDotDot, parts); // represent nDotdot as +/-.NNNNNE+/-N
        String mantissa = df5.format(parts[1]);
        signStr = (parts[0] < 0) ? "-" : " ";
        String exp = df1.format(parts[2]);
        if (exp.charAt(0) == '-') {
            card1sb.replace(44, 52, signStr + mantissa + exp);
        } else {
        	/** FIX: "0" in the exponent has a zero sign to get the checksums correct  */
        	if (exp.charAt(0) == '0') card1sb.replace(44, 52, signStr + mantissa + "-" + exp);
        	else card1sb.replace(44, 52, signStr + mantissa + "+" + exp);
        	// card1sb.replace(44, 52, signStr + mantissa + "+" + exp);
        }

        // B star
        formatInt(bstar, parts); // represent Bstar as +/-.NNNNNE+/-N
        mantissa = df5.format(parts[1]);
        signStr = (parts[0] < 0) ? "-" : " ";
        exp = df1.format(parts[2]);
        if (exp.charAt(0) == '-') {
            card1sb.replace(53, 61, signStr + mantissa + exp);
        } else {
        	/** FIX: "0" in the exponent has a zero sign to get the checksums correct  */
        	if (exp.charAt(0) == '0') card1sb.replace(53, 61, signStr + mantissa + "-" + exp);
        	else card1sb.replace(53, 61, signStr + mantissa + "+" + exp);
        	// card1sb.replace(53, 61, signStr + mantissa + "+" + exp);
        }

        // Ephemeris type
        String ephTypeStr = df1.format(ephemerisType);
        card1sb.replace(62, 63, ephTypeStr);

        // Elset num
        String elnoStr = df4.format(elsetNum);
        card1sb.replace(64, 68, elnoStr);

        card1sb.replace(68, 69, Integer.toString(checkSum(card1sb.substring(0,
                68))));
        card1 = new String(card1sb);

/** FIX: commented out the second card1 initialiser    */
//        card1 = new String(card1sb);
        return card1;
    }

    /**
     * This method returns the second line of the elset.
     * 
     * @return the String containing card2
     */
    public String getCard2() {

        String card2 = "2 00001  60.0000  90.0000 0010000  90.0000  90.0000 15.00000000    19";

        StringBuffer card2sb = new StringBuffer(card2);
        String satIDstr = df5.format(satID);
        card2sb.replace(2, 7, satIDstr);

        String incStr = df3dot4.format(inclination * TODEGREES);
        card2sb.replace(8, 16, incStr);

        String rtAscStr = df3dot4.format(rightAscension * TODEGREES);
        card2sb.replace(17, 25, rtAscStr);

        String eccStr = dfdot7.format(eccentricity);
        card2sb.replace(26, 33, eccStr.substring(1, 8));

        String argPerStr = df3dot4.format(argPerigee * TODEGREES);
        card2sb.replace(34, 42, argPerStr);

        String meanAnomStr = df3dot4.format(meanAnomaly * TODEGREES);
        card2sb.replace(43, 51, meanAnomStr);

        String meanMotionStr = df2dot8.format(meanMotion);
        card2sb.replace(52, 63, meanMotionStr);

        String revnumStr = df5.format(revNum);
        card2sb.replace(63, 68, revnumStr);

        // Compute the checksum
        card2sb.replace(68, 69, Integer.toString(checkSum(card2sb.substring(0,
                68))));
        card2 = new String(card2sb);

        return card2;
    }

    /**
     * This method returns the orbital eccentricity as a string.
     * 
     * @return the String containing the eccentricity (0.NNNNNNN) [0, <1]
     */
    public String getEccentricityString() {
        DecimalFormat dfe = new DecimalFormat(".0000000");
        return "0" + dfe.format(eccentricity);
    }

    /**
     * This method returns the orbital eccentricity.
     * 
     * @return the double containing the eccentricity [0.0, <1.0]
     */
    public double getEccentricity() {
        return eccentricity;
    }

    /**
     * This method returns the elset number.
     * 
     * @return the int containing the elset number [0,9999]
     */
    public int getElsetNum() {
        return elsetNum;
    }

    /**
     * This method returns the elset number as a string.
     * 
     * @return the String containing the elset number (NNNN) [0,9999]
     */
    public String getElsetNumString() {
        return df4.format(elsetNum);
    }

    /**
     * This method returns the ephemeris type (orbit model). 0 = SGP 2 = SGP4
     * 
     * @return the int containing the ephemeris type (N) [0(SGP) or 2(SGP4)]
     */
    public int getEphemerisType() {
        return ephemerisType;
    }

    /**
     * This method returns the ephemeris type (orbit model) as a string. 0 = SGP
     * 2 = SGP4
     * 
     * @return the String containing the ephemeris type (N) [0(SGP) or 2(SGP4)]
     */
    public String getEphemerisTypeString() {
        return Integer.toString(ephemerisType);
    }

    /**
     * This method returns the epoch day of the year.
     * 
     * @return the double containing the day of the epoch year [0,365.99999999]
     */
    public double getEpochDay() {
        return epochDay;
    }

    /**
     * This method returns the epoch day of the year as a string.
     * 
     * @return the String containing the day of the epoch year (DDD.DDDDDDD)
     *         [0,365.99999999]
     */
    public String getEpochDayString() {
        return df3dot8.format(epochDay);
    }

    /**
     * This method returns the epoch year.
     * 
     * @return the int containing the epoch year [0,99]
     */
    public int getEpochYr() {
        return epochYr;
    }

    /**
     * This method returns the epoch year as a string.
     * 
     * @return the String containing the epoch year (YY) [0,99]
     */
    public String getEpochYrString() {
        DecimalFormat df = new DecimalFormat("00");
        if (epochYr > 1900) {
            int tempYr = epochYr - 1900;
            if (tempYr > 100) {
                tempYr -= 100;
            }
            return df.format(tempYr);
        }
        return df.format(epochYr);

    }

    /**
     * This method returns the orbital inclination.
     * 
     * @return the double containing the inclination in radians
     */
    public double getInclination() {
        return inclination;
    }

    /**
     * This method returns the orbital inclination.
     * 
     * @return the double containing the inclination in degrees [0,180]
     */
    public double getInclinationDeg() {
        return inclination * TODEGREES;
    }

    /**
     * This method returns the orbital inclination as a string.
     * 
     * @return the String containing the inclination (NNN.NNNN) in degrees
     *         [0,180]
     */
    public String getInclinationString() {
        return df3dot4.format(inclination * TODEGREES);
    }

    /**
     * This method returns the satellite international designator as a string.
     * 
     * @return the String containing the international designator
     */
    public String getIntDesig() {
        return intDesig;
    }

    /**
     * This method returns the orbital mean anomaly.
     * 
     * @return the double containing the mean anomaly in radians
     */
    public double getMeanAnomaly() {
        return meanAnomaly;
    }

    /**
     * This method returns the orbital mean anomaly as a double
     * 
     * @return the double containing the mean anomaly (NNN.NNNN) in degrees
     *         [0,360]
     */
    public double getMeanAnomalyDeg() {
        return meanAnomaly * TODEGREES;
    }

    /**
     * This method returns the orbital mean anomaly as a string.
     * 
     * @return the String containing the mean anomaly (NNN.NNNN) in degrees
     *         [0,360]
     */
    public String getMeanAnomalyString() {
        return df3dot4.format(meanAnomaly * TODEGREES);
    }

    /**
     * This method returns the orbital mean motion.
     * 
     * @return the String containing the mean motion in revs/day [ < 17.0]
     */
    public double getMeanMotion() {
        return meanMotion;
    }

    /**
     * This method returns the orbital mean motion as a string.
     * 
     * @return the String containing the mean motion (NN.NNNNNNNN) in revs/day [ <
     *         17.0]
     */
    public String getMeanMotionString() {
        return df2dot8.format(meanMotion);
    }

    /**
     * This method returns the current satellite name string.
     * 
     * @return the String containing the sat name
     */
    public String getName() {
        return name;
    }

    /**
     * This method returns the orbital first-order drag term as a string.
     * 
     * @return the String containing the mean motion rate (S.NNNNNNNN) in
     *         revs/day^2
     */
    public double getNdot() {
        return nDot;
    }

    /**
     * This method returns the orbital second-order drag term as a string.
     * 
     * @return the String containing the mean motion acceleration (S.NNNNNESE)
     *         in revs/day^2
     */
    public String getNdotdotString() {
        String card1 = getCard1();
        if (" ".equals(card1.substring(50, 51))) {
            return card1.substring(44, 45) + "." + card1.substring(45, 50)
                    + "E+" + card1.substring(51, 52);
        }
        return card1.substring(44, 45) + "." + card1.substring(45, 50) + "E"
                + card1.substring(50, 52);

    }

    /**
     * This method returns the orbital second-order drag term.
     * 
     * @return the double containing the mean motion acceleration in revs/day^2
     */
    public double getNdotdot() {
        return nDotDot;
    }

    /**
     * This method returns the orbital first-order drag term as a string.
     * 
     * @return the String containing the mean motion rate (S.NNNNNNNN) in
     *         revs/day^2
     */
    public String getNdotString() {
        DecimalFormat dfnd = new DecimalFormat(".00000000");
        return dfnd.format(nDot);
    }

    /**
     * This method computes and returns the orbital period, in minutes/rev.
     * 
     * @return he double containing the orbital period (minutes)
     */
    public double getPeriod() {
        double mm = getMeanMotion(); // revs/day
        if (mm > 0.0) {
            return 1440.0 / mm;
        }

        return 0.0;
    }

    /**
     * This method returns the revolution number.
     * 
     * @return the int containing the revolution number [0,99999]
     */
    public int getRevNum() {
        return revNum;
    }

    /**
     * This method returns the revolution number as a string.
     * 
     * @return the String containing the revolution number (NNNNN) [0,99999]
     */
    public String getRevNumString() {
        return df5.format(revNum);
    }

    /**
     * This method returns the right ascension of the ascending node.
     * 
     * @return the double containing the right ascension in radians
     */
    public double getRightAscension() {
        return rightAscension;
    }

    /**
     * This method returns the right ascension of the ascending node.
     * 
     * @return the double containing the right ascension in degrees [0,360]
     */
    public double getRightAscensionDeg() {
        return rightAscension * TODEGREES;
    }

    /**
     * This method returns the right ascension of the ascending node as a
     * string.
     * 
     * @return the String containing the right ascension (NNN.NNNN) in degrees
     *         [0,360]
     */
    public String getRightAscensionString() {
        return df3dot4.format(rightAscension);
    }

    /**
     * This method returns the current,if any, satellite SSC#.
     * 
     * @return the int containing the sat ID
     */
    public int getSatID() {
        return satID;
    }

    /**
     * This method returns the current,if any, satellite SSC# as a string.
     * 
     * @return the String containing the sat ID [00000]
     */
    public String getSatIDString() {
        DecimalFormat df = new DecimalFormat("00000");
        return df.format(satID);
    }

    /**
     * This method returns the current,if any, satellite security classification
     * as a char.
     * 
     * @return the String containing the security class (U, C, S, or T)
     */
    public String getSecClass() {
        return satClass;
    }
    
    /**
     * Generates a hash code for the receiver. This method is supported
     * primarily for hash tables, such as those provided in java.util.
     * 
     * @return an integer hash code for the receiver
     * @see java.util.Hashtable
     */
    public int hashCode() {
        // Insert code to generate a hash code for the receiver here.
        // This implementation forwards the message to super. You may replace or
        // supplement this.
        // NOTE: if two objects are equal (equals(Object) returns true) they
        // must have the same hash code
        return (name + getCard1() + getCard2()).hashCode();
    }

    /**
     * This method returns true if the current 2-line elset contains valid data.
     * 
     * @return boolean
     */
    public boolean isValid() {
        return isValid;
    }

    /**
     * This method replaces the current,if any, lines of the elset.
     * 
     * @param card1 the String containing the replacement for line 1 of the
     *        elset
     * @param card2 the String containing the replacement for line 2 of the
     *        elset
     */
    public void set(String card1, String card2) throws SatElsetException,
            ValueOutOfRangeException {
        int slen = card1.length();
        String line1;
        String line2;
        if (slen <= 69) {
            line1 = card1 + BLANKCARD.substring(0, 69 - slen);
        } else {
            line1 = card1.substring(0, 70);
        }

        slen = card2.length();
        if (slen <= 69) {
            line2 = card2 + BLANKCARD.substring(0, 69 - slen);
        } else {
            line2 = card2.substring(0, 70);
        }

        isValid = card1IsValid(line1) && card2IsValid(line2)
                && line1.regionMatches(2, line2, 2, 5);
    }

    /**
     * This method replaces the current,if any, lines of the elset and the name.
     * 
     * @param name the String containing the replacement for the satellite name
     * @param card1 the String containing the replacement for line 1 of the
     *        elset
     * @param card2 the String containing the replacement for line 2 of the
     *        elset
     */
    public void set(String name, String card1, String card2)
            throws SatElsetException, ValueOutOfRangeException {
        if (name == null) {
            this.name = " ";
        } else {
            this.name = new String(name);
        }

        set(card1, card2);
    }

    /**
     * This method sets the orbital argument of perigee on card 2 to the value
     * supplied and updates the card2 checksum. The supplied value must be valid
     * ([0.0,360.0]) or false is returned.
     * 
     * @param argPer the double containing the argument of perigee to use
     *        [0.0,360.0] (deg)
     * @return boolean true if the value provided is valid
     */
    public boolean setArgPerigee(double argPer) throws ValueOutOfRangeException {
        if (argPer >= 0.0 && argPer <= TWOPI) {
            argPerigee = argPer;
            return true;
        }
        String errString = "SatElset.setArgPerigee value: [" + argPer
                + "] out of range [0-2Pi]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the Bstar field on card 1 to the value supplied. The
     * supplied value must be valid or false is returned.
     * 
     * @param Bstar the double containing the Bstar value to use
     */
    public void setBstar(double Bstar) {
        bstar = Bstar;
    }

    /**
     * This method replaces the current, if any, line 1 of the elset.
     * 
     * @param card the String containing the replacement for line 1 of the elset
     */
    public void setCard1(String card) throws SatElsetException,
            ValueOutOfRangeException {
        int slen = card.length();
        String line1;
        if (slen <= 69) {
            line1 = card + BLANKCARD.substring(0, 69 - slen);
        } else {
            line1 = card.substring(0, 70);
        }
        isValid = card1IsValid(line1);
    }

    /**
     * This method replaces the current, if any, line 2 of the elset.
     * 
     * @param card the String containing the replacement for line 2 of the elset
     */
    public void setCard2(String card) throws SatElsetException,
            ValueOutOfRangeException {
        int slen = card.length();
        String line2;
        if (slen <= 69) {
            line2 = card + BLANKCARD.substring(0, 69 - slen);
        } else {
            line2 = card.substring(0, 70);
        }

        isValid = card2IsValid(line2);
    }

    /**
     * This method sets the orbital eccentricity on card 2 to the value
     * supplied. The supplied value must be valid ([>0.0,0.9999999]).
     * 
     * @return boolean true if the value provided is valid
     * @param ecc the double containing the eccentricity to use [>0.0,0.9999999]
     */
    public boolean setEccentricity(double ecc) throws ValueOutOfRangeException {
        if (ecc > 0.0 && ecc < 1.0) {
            eccentricity = ecc;
            return true;
        }
        String errString = "SatElset.setEccentricity value: [" + ecc
                + "] out of range (0-1)";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the element number on card 1 to the value supplied. The
     * supplied value must be valid ([0,9999]) or false is returned.
     * 
     * @return boolean true if the value provided for element number is valid
     * @param elno the int containing the element to use [0,9999]
     */
    public boolean setElsetNum(int elno) throws ValueOutOfRangeException {
        if (elno >= 0 && elno < 10000) {
            elsetNum = elno;
            return true;
        }
        String errString = "SatElset.setElsetNum value: [" + elno
                + "] out of range [0-10000)";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the ephemeris type on card 1 to the value supplied. The
     * supplied value must be valid ([0 or 2]) or false is returned.
     * 
     * @return boolean true if the value provided for ephemeris type is valid
     * @param ephType the int containing the ephemeris type to use [0 or 2]
     */
    public boolean setEphemerisType(int ephType)
            throws ValueOutOfRangeException {
        if (ephType == 0 || ephType == 2) {
            ephemerisType = ephType;
            return true;
        }
        String errString = "SatElset.setEphemerisType value: [" + ephType
                + "] invalid [0,2]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the epoch day on card 1 to the value supplied. The
     * supplied value must be valid ([00.0,366.99999999]) or false is returned.
     * 
     * @return boolean true if the value provided for epoch day-of-the-year is
     *         valid
     * @param epochDay the double containing the epoch day to use
     *        [00.0,366.99999999]
     */
    public boolean setEpochDay(double epochDay) throws ValueOutOfRangeException {
        if (epochDay >= 0.0 && epochDay < 367.0) {
            this.epochDay = epochDay;
            return true;
        }
        String errString = "SatElset.setEpochDay value: [" + epochDay
                + "] out of range [0-367)";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the epoch year on card 1 to the value supplied. The
     * supplied value must be valid ([00,99]). if less than 100, the value is
     * converted to the correct 4 digit year.
     * 
     * @return boolean true if the value provided for epoch year is valid
     * @param epochYr the int containing the epoch year to use [00,99]
     */
    public boolean setEpochYr(int epochYr) {
        this.epochYr = epochYr;
        return true;
    }

    /**
     * This method sets the inclination on card 2 to the value supplied. The
     * supplied value must be valid ([0.0,180.0] degrees) or false is returned.
     * 
     * @return boolean true if the value provided for inclination is valid
     * @param inclin the double containing the inclination to use in radians
     */
    public boolean setInclination(double inclin)
            throws ValueOutOfRangeException {
        if (inclin >= 0.0 && inclin <= Math.PI) {
            inclination = inclin;
            return true;
        }
        String errString = "SatElset.setInclination value: [" + inclin
                + "] out of range [0-Pi]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method replaces the international designator on card 1 with the
     * string provided (up to 8 characters).
     * 
     * @return boolean true if the provided string was non-empty
     */
    public boolean setIntDes(String intDes) throws ValueOutOfRangeException {
        if (intDes == null) {
            String errString = "SatElset.setIntDes value: [" + intDes
                    + "] invalid [null]";
            throw new ValueOutOfRangeException(errString);
        }
        intDesig = intDes;
        return true;
    }

    /**
     * This method sets the elset mean anomaly on card 2 to the value supplied.
     * The supplied value must be valid ([0.0,360.0]) or false is returned.
     * 
     * @return boolean true if the value provided for mean anomaly is valid
     * @param meanAnom the double containing the mean anomaly to use radians
     */
    public boolean setMeanAnom(double meanAnom) throws ValueOutOfRangeException {
        if (meanAnom >= 0.0 && meanAnom <= TWOPI) {
            meanAnomaly = meanAnom;
            return true;
        }
        String errString = "SatElset.setMeanAnom value: [" + meanAnom
                + "] out of range [0-2Pi]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the elset mean motion on card 2 to the value supplied
     * and updates the card2 checksum. The supplied value must be valid
     * ([0.0,17.0]) or false is returned.
     * 
     * @return boolean true if the value provided for mean motion is valid
     * @param meanMotion the double containing the mean motion to use [0.0,17.0]
     *        (rev/day)
     */
    public boolean setMeanMotion(double meanMotion)
            throws ValueOutOfRangeException {
        if (meanMotion > 0.0 && meanMotion <= 17.0) {
            this.meanMotion = meanMotion;
            return true;
        }
        String errString = "SatElset.setMeanMotion value: [" + meanMotion
                + "] out of range [0-17]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method replaces the current satellite name string.
     * 
     * @param name the String containing the replacement sat name
     */
    public void setName(String name) {
        if (name == null) {
            this.name = " ";
        } else {
            this.name = new String(name);
        }
    }

    /**
     * This method sets the n dot (dn/dt) field on card 1 to the value supplied
     * and updates the checksum. The supplied value must be valid or false is
     * returned.
     * 
     * @return boolean true if the value provided for mean motion derivative is
     *         valid
     * @param nDot the double containing the n-dot to use (|nDot| < 1.0
     *        rev/day/day)
     */
    public boolean setNdot(double nDot) throws ValueOutOfRangeException {
        if (Math.abs(nDot) < 1.0) {
            this.nDot = nDot;
            return true;
        }
        String errString = "SatElset.setNdot value: [" + nDot
                + "] out of range [<1.0]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the n-dotdot ((dn/dt)/dt) field on card 1 to the value
     * supplied and updates the card 1 checksum. The supplied value must be
     * valid or false is returned.
     * 
     * @return boolean true if the value provided for mean motion second
     *         derivative is valid
     * @param nDotdot the double containing the n-dotdot value to use (|nDot| <
     *        1.0 rev/day^3)
     */
    public boolean setNdotdot(double nDotdot) {

        // FIXME range for nDotdot
        nDotDot = nDotdot;
        return true;

    }

    /**
     * This method sets the revolution count on card 2 to the value supplied and
     * updates the card 2 checksum. The supplied value must be valid ([0,99999])
     * or false is returned.
     * 
     * @return boolean true if the value provided for rev number is valid
     * @param revnum the int containing the rev number to use [0,99999]
     */
    public boolean setRevNum(int revnum) throws ValueOutOfRangeException {
        if (revnum >= 0 && revnum < 100000) {
            revNum = revnum;
            return true;
        }
        String errString = "SatElset.setRevNum value: [" + revNum
                + "] out of range [0-100000)";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the right ascension of the ascending node on card 2 to
     * the value supplied. The supplied value must be valid ([0.0,2Pi]
     * radians) or false is returned.
     * 
     * @return boolean true if the value provided for right ascension is valid
     * @param rtAsc the double containing the right ascension to use in radians
     */
    public boolean setRtAsc(double rtAsc) throws ValueOutOfRangeException {

         if (rtAsc >= 0.0 && rtAsc <= TWOPI) {
            rightAscension = rtAsc;
            return true;
        }
        String errString = "SatElset.setRtAsc value: [" + rtAsc
                + "] out of range [0-2Pi]";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method sets the satellite SSC# on one or both cards to the value
     * supplied and updates the checksums on both cards. The supplied value must
     * be valid ([1,99999]) or false is returned.
     * 
     * @return boolean true if the value provided for satID is valid
     * @param satID the int containing the sat ID to use
     */
    public boolean setSatID(int satID) throws ValueOutOfRangeException {
        // FUTURE 6-digit satellite number
        if (satID > 0 && satID < 100000) {
            this.satID = satID;
            return true;
        }
        String errString = "SatElset.setSatID value: [" + satID
                + "] out of range (0-100000)";
        throw new ValueOutOfRangeException(errString);
    }

    /**
     * This method replaces the security classification on card 1 with the
     * classification provided and updates the card 1 checksum.
     * 
     * @return boolean true if a valid classification was provided
     * @param secClass the String containing the new classification (U, C, S, or
     *        T)
     */
    public boolean setSecClass(String secClass) throws ValueOutOfRangeException {
        if ("U".equals(secClass) || "C".equals(secClass)
                || "S".equals(secClass) || "T".equals(secClass)) {
            satClass = secClass;
            return true;
        }
        String errString = "SatElset.setSecClass value: [" + secClass
                + "] out of range [U,C,S,T])";
        throw new ValueOutOfRangeException(errString);
    }
}