package sgp4v;

/**
 * Exception for SatElset problems
 * 
 * @author Joe Coughlin
 *
 */
public class SatElsetException extends Exception {

    /**
     * SatElsetException constructor
     */
    public SatElsetException(){
        super();
    }
    
    /**
     * SatElsetException constructor
     * @param s
     */
    public SatElsetException(String s){
        super(s);
    }
    
    /**
     * Serialization ID to elminate compiler warnings
     */
    private static final long serialVersionUID = -773728672867176869L;

}
