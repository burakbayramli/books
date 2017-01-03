package ifi.in104;

import java.net.URL;
import java.io.StreamTokenizer;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Vector;

/**
 * Stores the quotes for a single company. This class can also read
 * data from an http server. The data on the server is supposed to
 * be on the form ...
 */

public class StockData
{
  /**
   * Creates a stockdata object for the company with ticker t,
   * on the exchange e.
   */

  public StockData(String e, String t, String n)
  {
    exchange = e;
    ticker = t;
    company_name = n;
  }

  /**
   * Get the full name of this company. This will return a string
   * with zero length if the readQuotesFromServer has not been
   * called sucsessfully.
   */

  public String getCompanyName()
  {
    return company_name;
  }

  /**
   * Read the quotes from the http server www.ifi.uio.no.
   */

  public void readQuotesFromServer() throws Exception
  {
    URL url = new URL("http://www.ifi.uio.no/~stensby/"+exchange+"/"+ticker);

    StreamTokenizer in = new StreamTokenizer(
			    new BufferedReader(
                               new InputStreamReader(url.openStream())));

    in.resetSyntax();
    in.whitespaceChars(0,32);
    in.wordChars(33,255);
    in.eolIsSignificant(true);

    // Read the single quotes and store in vector quotes.

    int pos=0;  // Current position on current line.

    String curr_date=""; // Temporary storage
    double curr_price=0.0;
    int    curr_volume=0;

    quotes = new Vector(365,31);

    int token_type = in.nextToken();
    while(token_type != StreamTokenizer.TT_EOF)
    {
      // Update position
      pos++;

      if(token_type==StreamTokenizer.TT_EOL)
      {
	SingleQuote k = new SingleQuote(curr_date, curr_price, curr_volume);
	quotes.addElement(k);

	pos = 0;  // Reset position
      }

      if(pos==1)
      {
	// Each line starts with a date.

 	if(token_type==StreamTokenizer.TT_WORD)
	  curr_date = in.sval;
	else
	{
	  throw new Exception("Expected date string, line="+in.lineno()+
			      ", pos="+pos);
	}
      }

      if(pos==2)
      {
	// The second element is the end of day price for a company
	// and the index value for an index.

	if(token_type==StreamTokenizer.TT_WORD)
	{
	  try
	  {
	    curr_price = Double.valueOf(in.sval).doubleValue();
	  }
	  catch(NumberFormatException e)
	  {
	    throw new Exception("Expected a number");
	  }
	}
	else
	  throw new Exception("Expected a number");
      }

      if(pos==3)
      {
	has_volume=true;

	// The third element is the volume for a company

	if(token_type==StreamTokenizer.TT_WORD)
	{
	  try
	  {
	    curr_volume = Integer.valueOf(in.sval).intValue()/1000;
	  }
	  catch(NumberFormatException e)
	  {
	    throw new Exception("Expected a number");
	  }
	}
	else
	  throw new Exception("Expected a number");
      }

      // Get next token

      token_type = in.nextToken();
    }
  }

  /**
   * Return an array with the day identifiers transformed to
   * doubles. This array is suitable as the x-values in a plot.
   */

  public double[] getDayIdentifiers(String start, String stop)
  {
    // Create two dummy quotes in order to pick correct subset

    int start_id = new SingleQuote(start, 0.0).getDayId();
    int stop_id  = new SingleQuote(stop, 0.0).getDayId();

    // Count number of quotes that are within desired range

    int no=0;

    int i;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) no++;
    }

    // Copy data

    double x[] = new double[no];

    int indx=0;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) 
	x[indx++] = (double) day_id;
    }

    return x;
  }

  /**
   * Return an array with the prices. This array is suitable as
   * the y-values in a plot.
   */

  public double[] getPrices(String start, String stop)
  {
    // Create two dummy quotes in order to pick correct subset

    int start_id = new SingleQuote(start, 0.0).getDayId();
    int stop_id  = new SingleQuote(stop, 0.0).getDayId();

    // Count number of quotes that are within desired range

    int no=0;

    int i;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) no++;
    }

    // Copy data

    double y[] = new double[no];

    int indx=0;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) 
	y[indx++] = (double) (((SingleQuote) quotes.elementAt(i)).getPrice());
    }

    return y;
  }

  /**
   *
   */

  public boolean hasVolume()
  {
    return has_volume;
  }

  /**
   * Return an array with the volumes transformed to doubles.
   * This array is suitable as the y-values in a plot.
   */

  public double[] getVolumes(String start, String stop)
  {
    // Create two dummy quotes in order to pick correct subset

    int start_id = new SingleQuote(start, 0.0).getDayId();
    int stop_id  = new SingleQuote(stop, 0.0).getDayId();

    // Count number of quotes that are within desired range

    int no=0;

    int i;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) no++;
    }

    // Copy data

    double y[] = new double[no];

    int indx=0;
    for(i=0 ; i < quotes.size() ; i++)
    {
      int day_id = (((SingleQuote) quotes.elementAt(i)).getDayId());

      if(day_id>=start_id && day_id <=stop_id) 
	y[indx++] = (double) (((SingleQuote) quotes.elementAt(i)).getVolume());
    }

    return y;
  }

  String exchange;
  String ticker;
  String company_name;

  boolean has_volume=false;

  Vector quotes;
}
