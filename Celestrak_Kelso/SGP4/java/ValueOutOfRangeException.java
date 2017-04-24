package sgp4v;

/**
 * Thrown when a value is out of the valid range.
 * 
 * @author Joe Coughlin
 * @version 1.0
 * 
 * @since 1.0
 */
public class ValueOutOfRangeException extends Exception {
    private final static long serialVersionUID = 7729000960933265858L;

    /**
     * ValueOutOfRangeException constructor
     */
    public ValueOutOfRangeException() {
        super();
    }

    /**
     * ValueOutOfRangeException constructor
     * 
     * @param s java.lang.String
     */
    public ValueOutOfRangeException(String s) {
        super(s);
    }
}