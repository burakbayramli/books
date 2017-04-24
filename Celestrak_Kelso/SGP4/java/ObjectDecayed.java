package sgp4v;

/**
 * Exception raised when a space object's orbit has decayed and
 * it can no longer be propagated.
 */
public class ObjectDecayed extends SatElsetException
{
    /**
     * 
     */
    private static final long serialVersionUID = 1230517552210764538L;

    /** Constructor. */
    public ObjectDecayed()
    {
        super();
    }

    /**
     * Constructor.
     * @param msg message string
     */
    public ObjectDecayed(String msg)
    {
        super(msg);
    }
}