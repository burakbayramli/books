package ifi.in104;

import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;

/**
 * Stores information about a single quote. This information
 * consists of a date (implicitly through a day identifier),
 * the value of the stock the end of that day and the total volume
 * of stocks traded on this day.
 */

public class SingleQuote
{
  /**
   * Create a SingleQuote object for the specified date and the
   * specified end of day price. The date must be on the form
   * dd.mm.yy.
   */

  public SingleQuote(String date, double p)
  {
    price = p;
    parseDate(date);
  }

  /**
   * Create a SingleQuote object for the specified date, the
   * specified end of day price and the specified volume.
   * The date must be on the form
   * dd.mm.yy.
   */

  public SingleQuote(String date, double p, int v)
  {
    price = p;
    volume = v;
    parseDate(date);
  }

  /**
   * Get day identifier. The identifier is the number of days
   * since January 1, 1997.
   */

  public int getDayId()
  {
    return day_ident;
  }

  /**
   * Get the end of day price.
   */

  public double getPrice()
  {
    return price;
  }

  /**
   * Get the number of stocks traded.
   */

  public int getVolume()
  {
    return volume;
  }

  /**
   * Parse date and calculate the day identifier.
   */

  protected void parseDate(String date)
  {
    try
    {
      Date epoc = parser.parse("01.01.97");
      Date curr = parser.parse(date);

      day_ident = (int) ((curr.getTime()-epoc.getTime())/millisec_in_a_day);
    }
    catch (ParseException e)
    {
      System.out.println(e.getMessage());
      day_ident = -99999;
      price   = -999.0;
      volume  = -999;
    }
  }

  // Private members

  private int    day_ident;
  private double price;
  private int    volume;

  // Some constants used by this class
  private static final SimpleDateFormat parser
    = new SimpleDateFormat("dd.MM.yy");
  private static final int millisec_in_a_day = 86400000;
}
