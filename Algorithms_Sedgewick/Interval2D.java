/*************************************************************************
 *  Compilation:  javac Interval2D.java
 *  Execution:    java Interval2D
 *  
 *  2-dimensional interval data type.
 *
 *************************************************************************/

/**
 *  The <tt>Interval2D</tt> class represents a closed two-dimensional interval,
 *  which represents all points (x, y) with both xleft <= x <= xright and
 *  yleft <= y <= right.
 *  Two-dimensional intervals are immutable: their values cannot be changed
 *  after they are created.
 *  The class <code>Interval2D</code> includes methods for checking whether
 *  a two-dimensional interval contains a point and determining whether
 *  two two-dimensional intervals intersect.
 *  <p>
 *  For additional documentation, see <a href="/algs4/12oop">Section 1.2</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Interval2D {
    private final Interval1D x;
    private final Interval1D y;

    /**
     * Initializes a two-dimensional interval.
     * @param x the one-dimensional interval of x-coordinates
     * @param y the one-dimensional interval of y-coordinates
     */
    public Interval2D(Interval1D x, Interval1D y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Does this two-dimensional interval intersect that two-dimensional interval?
     * @param that the other two-dimensional interval
     * @return true if this two-dimensional interval intersects
     *    that two-dimensional interval; false otherwise
     */
    public boolean intersects(Interval2D that) {
        if (!this.x.intersects(that.x)) return false;
        if (!this.y.intersects(that.y)) return false;
        return true;
    }

    /**
     * Does this two-dimensional interval contain the point p?
     * @param p the two-dimensional point
     * @return true if this two-dimensional interval contains the point p; false otherwise
     */
    public boolean contains(Point2D p) {
        return x.contains(p.x())  && y.contains(p.y());
    }

    /**
     * Returns the area of this two-dimensional interval.
     * @return the area of this two-dimensional interval
     */
    public double area() {
        return x.length() * y.length();
    }
        
    /**
     * Returns a string representation of this two-dimensional interval.
     * @return a string representation of this two-dimensional interval
     *    in the form [xleft, xright] x [yleft, yright]
     */
    public String toString() {
        return x + " x " + y;
    }

    /**
     * Draws this two-dimensional interval to standard draw.
     */
    public void draw() {
        double xc = (x.left() + x.right()) / 2.0;
        double yc = (y.left() + y.right()) / 2.0;
        StdDraw.rectangle(xc, yc, x.length() / 2.0, y.length() / 2.0);
    }

    /**
     * Unit tests the <tt>Interval2D</tt> data type.
     */
    public static void main(String[] args) {
        double xlo = Double.parseDouble(args[0]);
        double xhi = Double.parseDouble(args[1]);
        double ylo = Double.parseDouble(args[2]);
        double yhi = Double.parseDouble(args[3]);
        int T = Integer.parseInt(args[4]);

        Interval1D xinterval = new Interval1D(xlo, xhi);
        Interval1D yinterval = new Interval1D(ylo, yhi);
        Interval2D box = new Interval2D(xinterval, yinterval);
        box.draw();

        Counter counter = new Counter("hits");
        for (int t = 0; t < T; t++) {
            double x = StdRandom.uniform(0.0, 1.0);
            double y = StdRandom.uniform(0.0, 1.0);
            Point2D p = new Point2D(x, y);

            if (box.contains(p)) counter.increment();
            else                 p.draw();
        }

        StdOut.println(counter);
        StdOut.printf("box area = %.2f\n", box.area());
    }
}
