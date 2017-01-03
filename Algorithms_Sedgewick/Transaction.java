/*************************************************************************
 *  Compilation:  javac Transaction.java
 *  Execution:    java Transaction
 *  
 *  Data type for commercial transactions.
 *
 *************************************************************************/

import java.util.Arrays;
import java.util.Comparator;


/**
 *  The <tt>Transaction</tt> class is an immutable data type to encapsulate a
 *  commercial transaction with a customer name, date, and amount.
 *  <p>
 *  For additional documentation, see <a href="/algs4/12oop">Section 1.2</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Transaction implements Comparable<Transaction> {
    private final String  who;      // customer
    private final Date    when;     // date
    private final double  amount;   // amount


    /**
     * Initializes a new transaction from the given arguments.
     * @param who the person involved in the transaction
     * @param when the date of the transaction
     * @param amount the amount of the transaction
     * @throws IllegalArgumentException if <tt>amount</tt> 
     *    is <tt>Double.NaN</tt>, <tt>Double.POSITIVE_INFINITY</tt> or
     *    <tt>Double.NEGATIVE_INFINITY</tt>
     */

    public Transaction(String who, Date when, double amount) {
        if (Double.isNaN(amount) || Double.isInfinite(amount))
            throw new IllegalArgumentException("Amount cannot be NaN or infinite");
        this.who    = who;
        this.when   = when;
        if (amount == 0.0) this.amount = 0.0;  // to handle -0.0
        else               this.amount = amount;
    }

    /**
     * Initializes a new transaction by parsing a string of the form NAME DATE AMOUNT.
     * @param transaction the string to parse
     * @throws IllegalArgumentException if <tt>amount</tt> 
     *    is <tt>Double.NaN</tt>, <tt>Double.POSITIVE_INFINITY</tt> or
     *    <tt>Double.NEGATIVE_INFINITY</tt>
     */
    public Transaction(String transaction) {
        String[] a = transaction.split("\\s+");
        who    = a[0];
        when   = new Date(a[1]);
        double value = Double.parseDouble(a[2]);
        if (value == 0.0) amount = 0.0;  // convert -0.0 0.0
        else              amount = value;
        if (Double.isNaN(amount) || Double.isInfinite(amount))
            throw new IllegalArgumentException("Amount cannot be NaN or infinite");
    }

    /**
     * Returns the name of the customer involved in the transaction.
     * @return the name of the customer involved in the transaction
     */
    public String who() {
        return who;
    }
 
    /**
     * Returns the date of the transaction.
     * @return the date of the transaction
     */
    public Date when() {
        return when;
    }
 
    /**
     * Returns the amount of the transaction.
     * @return the amount of the transaction
     */
    public double amount() {
       return amount;
    }

    /**
     * Returns a string representation of the transaction.
     * @return a string representation of the transaction
     */
    public String toString() {
        return String.format("%-10s %10s %8.2f", who, when, amount);
    }

    /**
     * Compares this transaction to that transaction.
     * @return { a negative integer, zero, a positive integer}, depending
     *    on whether the amount of this transaction is { less than,
     *    equal to, or greater than } the amount of that transaction
     */
    public int compareTo(Transaction that) {
        if      (this.amount < that.amount) return -1;
        else if (this.amount > that.amount) return +1;
        else                                return  0;
    }    

    /**
     * Is this transaction equal to x?
     * @param x the other transaction
     * @return true if this transaction is equal to x; false otherwise
     */
    public boolean equals(Object x) {
        if (x == this) return true;
        if (x == null) return false;
        if (x.getClass() != this.getClass()) return false;
        Transaction that = (Transaction) x;
        return (this.amount == that.amount) && (this.who.equals(that.who))
                                            && (this.when.equals(that.when));
    }


    /**
     * Returns a hash code for this transaction.
     * @return a hash code for this transaction
     */
    public int hashCode() {
        int hash = 17;
        hash = 31*hash + who.hashCode();
        hash = 31*hash + when.hashCode();
        hash = 31*hash + ((Double) amount).hashCode();
        return hash;
    }

    /**
     * Compares two transactions by customer name.
     */
    public static class WhoOrder implements Comparator<Transaction> {
        public int compare(Transaction v, Transaction w) {
            return v.who.compareTo(w.who);
        }
    }

    /**
     * Compares two transactions by date.
     */
    public static class WhenOrder implements Comparator<Transaction> {
        public int compare(Transaction v, Transaction w) {
            return v.when.compareTo(w.when);
        }
    }

    /**
     * Compares two transactions by amount.
     */
    public static class HowMuchOrder implements Comparator<Transaction> {
        public int compare(Transaction v, Transaction w) {
            if      (v.amount < w.amount) return -1;
            else if (v.amount > w.amount) return +1;
            else                          return  0;
        }
    }


    /**
     * Unit tests the transaction data type.
     */
    public static void main(String[] args) {
        Transaction[] a = new Transaction[4];
        a[0] = new Transaction("Turing   6/17/1990  644.08");
        a[1] = new Transaction("Tarjan   3/26/2002 4121.85");
        a[2] = new Transaction("Knuth    6/14/1999  288.34");
        a[3] = new Transaction("Dijkstra 8/22/2007 2678.40");

        StdOut.println("Unsorted");
        for (int i = 0; i < a.length; i++)
            StdOut.println(a[i]);
        StdOut.println();
        
        StdOut.println("Sort by date");
        Arrays.sort(a, new Transaction.WhenOrder());
        for (int i = 0; i < a.length; i++)
            StdOut.println(a[i]);
        StdOut.println();

        StdOut.println("Sort by customer");
        Arrays.sort(a, new Transaction.WhoOrder());
        for (int i = 0; i < a.length; i++)
            StdOut.println(a[i]);
        StdOut.println();

        StdOut.println("Sort by amount");
        Arrays.sort(a, new Transaction.HowMuchOrder());
        for (int i = 0; i < a.length; i++)
            StdOut.println(a[i]);
        StdOut.println();
    }

}

