/*************************************************************************
 *  Compilation:  javac Vector.java
 *  Execution:    java Vector
 *
 *  Implementation of a vector of real numbers.
 *
 *  This class is implemented to be immutable: once the client program
 *  initialize a Vector, it cannot change any of its fields
 *  (N or data[i]) either directly or indirectly. Immutability is a
 *  very desirable feature of a data type.
 *
 *  % java Vector
 *     x     = [ 1.0 2.0 3.0 4.0 ]
 *     y     = [ 5.0 2.0 4.0 1.0 ]
 *     z     = [ 6.0 4.0 7.0 5.0 ]
 *   10z     = [ 60.0 40.0 70.0 50.0 ]
 *    |x|    = 5.477225575051661
 *   <x, y>  = 25.0
 * 
 *
 *  Note that Vector is also the name of an unrelated Java library class.
 *
 *************************************************************************/

/**
 *  The <tt>Vector</tt> class represents a <em>d</em>-dimensional mathematical vector.
 *  Vectors are immutable: their values cannot be changed after they are created.
 *  The class <code>Vectors</code> includes methods for addition, subtraction,
 *  dot product, scalar product, unit vector, Euclidean distance, and
 *  Euclidean norm.
 *  <p>
 *  For additional documentation, see <a href="/algs4/12oop">Section 1.2</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Vector { 

    private int N;               // length of the vector
    private double[] data;       // array of vector's components


    /**
     * Initializes a d-dimensional zero vector.
     * @param d the dimension of the vector
     */
    public Vector(int d) {
        N = d;
        data = new double[N];
    }

    /**
     * Initializes a vector from either an array or a vararg list.
     * The vararg syntax supports a constructor that takes a variable number of
     * arugments such as Vector x = new Vector(1.0, 2.0, 3.0, 4.0).
     * @param a the array or vararg list
     */
    public Vector(double... a) {
        N = a.length;

        // defensive copy so that client can't alter our copy of data[]
        data = new double[N];
        for (int i = 0; i < N; i++)
            data[i] = a[i];
    }

    /**
     * Returns the length of this vector.
     * @return the dimension of this vector
     */
    public int length() {
        return N;
    }

    /**
     * Returns the inner product of this vector with that vector.
     * @throws IllegalArgumentException if the lengths of the two vectors are not equal.
     * @param that the other vector
     * @return the dot product between this vector and that vector
     */
    public double dot(Vector that) {
        if (this.N != that.N) throw new IllegalArgumentException("Dimensions don't agree");
        double sum = 0.0;
        for (int i = 0; i < N; i++)
            sum = sum + (this.data[i] * that.data[i]);
        return sum;
    }

    /**
     * Returns the Euclidean norm of this vector.
     * @return the Euclidean norm of this vector
     */
    public double magnitude() {
        return Math.sqrt(this.dot(this));
    }

    /**
     * Returns the Euclidean distance between this vector and that vector.
     * @throws IllegalArgumentException if the lengths of the two vectors are not equal.
     * @param that the other vector 
     * @return the Euclidean distance between this vector and that vector
     */
    public double distanceTo(Vector that) {
        if (this.N != that.N) throw new IllegalArgumentException("Dimensions don't agree");
        return this.minus(that).magnitude();
    }

    /**
     * Returns the sum of this vector and that vector: this + that.
     * @throws IllegalArgumentException if the lengths of the two vectors are not equal.
     * @param that the vector to add to this vector
     * @return the sum of this vector and that vector
     */
    public Vector plus(Vector that) {
        if (this.N != that.N) throw new IllegalArgumentException("Dimensions don't agree");
        Vector c = new Vector(N);
        for (int i = 0; i < N; i++)
            c.data[i] = this.data[i] + that.data[i];
        return c;
    }

    /**
     * Returns the difference between this vector and that vector: this - that.
     * @param that the vector to subtract from this vector
     * @return the difference between this vector and that vector
     * @throws IllegalArgumentException if the lengths of the two vectors are not equal.
     */
    public Vector minus(Vector that) {
        if (this.N != that.N) throw new IllegalArgumentException("Dimensions don't agree");
        Vector c = new Vector(N);
        for (int i = 0; i < N; i++)
            c.data[i] = this.data[i] - that.data[i];
        return c;
    }

    /**
     * Returns the ith cartesian coordinate.
     * @param i the coordinate index
     * @return the ith cartesian coordinate
     */
    public double cartesian(int i) {
        return data[i];
    }

    /**
     * Returns the product of this factor multiplied by the scalar factor: this * factor.
     * @param factor the multiplier
     * @return the scalar product of this vector and factor
     */
    public Vector times(double factor) {
        Vector c = new Vector(N);
        for (int i = 0; i < N; i++)
            c.data[i] = factor * data[i];
        return c;
    }

    /**
     * Returns a unit vector in the direction of this vector.
     * @return a unit vector in the direction of this vector
     * @throws ArithmeticException if this vector is the zero vector.
     */
    public Vector direction() {
        if (this.magnitude() == 0.0) throw new ArithmeticException("Zero-vector has no direction");
        return this.times(1.0 / this.magnitude());
    }


    /**
     * Returns a string representation of this vector.
     * @return a string representation of this vector, which consists of the 
     *    the vector entries, separates by single spaces
     */
    public String toString() {
        String s = "";
        for (int i = 0; i < N; i++)
            s = s + data[i] + " ";
        return s;
    }

    /**
     * Unit tests the data type methods.
     */
    public static void main(String[] args) {
        double[] xdata = { 1.0, 2.0, 3.0, 4.0 };
        double[] ydata = { 5.0, 2.0, 4.0, 1.0 };
        Vector x = new Vector(xdata);
        Vector y = new Vector(ydata);

        StdOut.println("   x       = " + x);
        StdOut.println("   y       = " + y);

        Vector z = x.plus(y);
        StdOut.println("   z       = " + z);

        z = z.times(10.0);
        StdOut.println(" 10z       = " + z);

        StdOut.println("  |x|      = " + x.magnitude());
        StdOut.println(" <x, y>    = " + x.dot(y));
        StdOut.println("dist(x, y) = " + x.distanceTo(y));
        StdOut.println("dir(x)     = " + x.direction());

    }
}
