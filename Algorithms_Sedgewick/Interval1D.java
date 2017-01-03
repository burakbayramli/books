/*************************************************************************
 *  Compilation:  javac Interval1D.java
 *  Execution:    java Interval1D
 *  
 *  1-dimensional interval data type.
 *
 *************************************************************************/

import java.util.Arrays;
import java.util.Comparator;

/**
 *  The <tt>Interval1D</tt> class represents a one-dimensional closed interval.
 *  Intervals are immutable: their values cannot be changed after they are created.
 *  The class <code>Interval1D</code> includes methods for checking whether
 *  an interval contains a point and determining whether two intervals intersect.
 *  <p>
 *  For additional documentation, see <a href="/algs4/12oop">Section 1.2</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Interval1D {

    /**
     * Compares two intervals by left endpoint.
     */
    public static final Comparator<Interval1D> LEFT_ENDPOINT_ORDER  = new LeftComparator();

    /**
     * Compares two intervals by right endpoint.
     */
    public static final Comparator<Interval1D> RIGHT_ENDPOINT_ORDER = new RightComparator();

    /**
     * Compares two intervals by length.
     */
    public static final Comparator<Interval1D> LENGTH_ORDER = new LengthComparator();

    private final double left;
    private final double right;

    /**
     * Initializes an interval [left, right].
     * @param left the left endpoint
     * @param right the right endpoint
     * @throws IllegalArgumentException if the left endpoint is greater than the right endpoint
     * @throws IllegalArgumentException if either <tt>left</tt> or <tt>right</tt>
     *    is <tt>Double.NaN</tt>, <tt>Double.POSITIVE_INFINITY</tt> or
     *    <tt>Double.NEGATIVE_INFINITY</tt>

     */
    public Interval1D(double left, double right) {
        if (Double.isInfinite(left) || Double.isInfinite(right))
            throw new IllegalArgumentException("Endpoints must be finite");
        if (Double.isNaN(left) || Double.isNaN(right))
            throw new IllegalArgumentException("Endpoints cannot be NaN");

        if (left <= right) {
            this.left  = left;
            this.right = right;
        }
        else throw new IllegalArgumentException("Illegal interval");
    }

    /**
     * Returns the left endpoint.
     * @return the left endpoint
     */
    public double left() { 
        return left;
    }

    /**
     * Returns the right endpoint.
     * @return the right endpoint
     */
    public double right() { 
        return right;
    }

    /**
     * Does this interval intersect that interval?
     * @param that the other interval
     * @return true if this interval intersects that interval; false otherwise
     */
    public boolean intersects(Interval1D that) {
        if (this.right < that.left) return false;
        if (that.right < this.left) return false;
        return true;
    }

    /**
     * Does this interval contain the value x?
     * @param x the value
     * @return true if this interval contains the value x; false otherwise
     */
    public boolean contains(double x) {
        return (left <= x) && (x <= right);
    }

    /**
     * Returns the length of this interval.
     * @return the length of this interval (right - left)
     */
    public double length() {
        return right - left;
    }

    /**
     * Returns a string representation of this interval.
     * @return a string representation of this interval in the form [left, right]
     */
    public String toString() {
        return "[" + left + ", " + right + "]";
    }



    // ascending order of left endpoint, breaking ties by right endpoint
    private static class LeftComparator implements Comparator<Interval1D> {
        public int compare(Interval1D a, Interval1D b) {
            if      (a.left  < b.left)  return -1;
            else if (a.left  > b.left)  return +1;
            else if (a.right < b.right) return -1;
            else if (a.right > b.right) return +1;
            else                        return  0;
        }
    }

    // ascending order of right endpoint, breaking ties by left endpoint
    private static class RightComparator implements Comparator<Interval1D> {
        public int compare(Interval1D a, Interval1D b) {
            if      (a.right < b.right) return -1;
            else if (a.right > b.right) return +1;
            else if (a.left  < b.left)  return -1;
            else if (a.left  > b.left)  return +1;
            else                        return  0;
        }
    }

    // ascending order of length
    private static class LengthComparator implements Comparator<Interval1D> {
        public int compare(Interval1D a, Interval1D b) {
            double alen = a.length();
            double blen = b.length();
            if      (alen < blen) return -1;
            else if (alen > blen) return +1;
            else                  return  0;
        }
    }




    /**
     * Unit tests the <tt>Interval1D</tt> data type.
     */
    public static void main(String[] args) {
        Interval1D[] intervals = new Interval1D[4];
        intervals[0] = new Interval1D(15.0, 33.0);
        intervals[1] = new Interval1D(45.0, 60.0);
        intervals[2] = new Interval1D(20.0, 70.0);
        intervals[3] = new Interval1D(46.0, 55.0);

        StdOut.println("Unsorted");
        for (int i = 0; i < intervals.length; i++)
            StdOut.println(intervals[i]);
        StdOut.println();
        
        StdOut.println("Sort by left endpoint");
        Arrays.sort(intervals, Interval1D.LEFT_ENDPOINT_ORDER);
        for (int i = 0; i < intervals.length; i++)
            StdOut.println(intervals[i]);
        StdOut.println();

        StdOut.println("Sort by right endpoint");
        Arrays.sort(intervals, Interval1D.RIGHT_ENDPOINT_ORDER);
        for (int i = 0; i < intervals.length; i++)
            StdOut.println(intervals[i]);
        StdOut.println();

        StdOut.println("Sort by length");
        Arrays.sort(intervals, Interval1D.LENGTH_ORDER);
        for (int i = 0; i < intervals.length; i++)
            StdOut.println(intervals[i]);
        StdOut.println();
    }
}
