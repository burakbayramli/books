/*************************************************************************
 *  Compilation:  javac Counter.java
 *  Execution:    java Counter N T
 *  Dependencies: StdRandom.java StdOut.java
 *
 *  A mutable data type for an integer counter.
 *
 *  The test clients create N counters and performs T increment
 *  operations on random counters.
 *
 *  % java Counter 6 600000
 *  0: 99870
 *  1: 99948
 *  2: 99738
 *  3: 100283
 *  4: 100185
 *  5: 99976
 *
 *************************************************************************/

/**
 *  The <tt>Counter</tt> class is a mutable data type to encapsulate a counter.
 *  <p>
 *  For additional documentation, see <a href="/algs4/12oop">Section 1.2</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Counter implements Comparable<Counter> {

    private final String name;     // counter name
    private int count = 0;         // current value

    /**
     * Initializes a new counter starting at 0, with the given id.
     * @param id the name of the counter
     */
    public Counter(String id) {
        name = id;
    } 

    /**
     * Increments the counter by 1.
     */
    public void increment() {
        count++;
    } 

    /**
     * Returns the current count.
     */
    public int tally() {
        return count;
    } 

    /**
     * Returns a string representation of this counter
     */
    public String toString() {
        return count + " " + name;
    } 

    /**
     * Compares this counter to that counter.
     */
    public int compareTo(Counter that) {
        if      (this.count < that.count) return -1;
        else if (this.count > that.count) return +1;
        else                              return  0;
    }


    /**
     * Reads two command-line integers N and T; creates N counters;
     * increments T counters at random; and prints results.
     */
    public static void main(String[] args) { 
        int N = Integer.parseInt(args[0]);
        int T = Integer.parseInt(args[1]);

        // create N counters
        Counter[] hits = new Counter[N];
        for (int i = 0; i < N; i++) {
            hits[i] = new Counter("counter" + i);
        }

        // increment T counters at random
        for (int t = 0; t < T; t++) {
            hits[StdRandom.uniform(N)].increment();
        }

        // print results
        for (int i = 0; i < N; i++) {
            StdOut.println(hits[i]);
        }
    } 
} 
