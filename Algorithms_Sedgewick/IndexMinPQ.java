/*************************************************************************
 *  Compilation:  javac IndexMinPQ.java
 *  Execution:    java IndexMinPQ
 *
 *  Minimum-oriented indexed PQ implementation using a binary heap.
 *
 *********************************************************************/

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 *  The <tt>IndexMinPQ</tt> class represents an indexed priority queue of generic keys.
 *  It supports the usual <em>insert</em> and <em>delete-the-minimum</em>
 *  operations, along with <em>delete</em> and <em>change-the-key</em> 
 *  methods. In order to let the client refer to keys on the priority queue,
 *  an integer between 0 and NMAX-1 is associated with each key&mdash;the client
 *  uses this integer to specify which key to delete or change.
 *  It also supports methods for peeking at the minimum key,
 *  testing if the priority queue is empty, and iterating through
 *  the keys.
 *  <p>
 *  This implementation uses a binary heap along with an array to associate
 *  keys with integers in the given range.
 *  The <em>insert</em>, <em>delete-the-minimum</em>, <em>delete</em>,
 *  <em>change-key</em>, <em>decrease-key</em>, and <em>increase-key</em>
 *  operations take logarithmic time.
 *  The <em>is-empty</em>, <em>size</em>, <em>min-index</em>, <em>min-key</em>, and <em>key-of</em>
 *  operations take constant time.
 *  Construction takes time proportional to the specified capacity.
 *  <p>
 *  For additional documentation, see <a href="http://algs4.cs.princeton.edu/24pq">Section 2.4</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class IndexMinPQ<Key extends Comparable<Key>> implements Iterable<Integer> {
    private int NMAX;        // maximum number of elements on PQ
    private int N;           // number of elements on PQ
    private int[] pq;        // binary heap using 1-based indexing
    private int[] qp;        // inverse of pq - qp[pq[i]] = pq[qp[i]] = i
    private Key[] keys;      // keys[i] = priority of i

    /**
     * Initializes an empty indexed priority queue with indices between 0 and NMAX-1.
     * @param NMAX the keys on the priority queue are index from 0 to NMAX-1
     * @throws java.lang.IllegalArgumentException if NMAX < 0
     */
    public IndexMinPQ(int NMAX) {
        if (NMAX < 0) throw new IllegalArgumentException();
        this.NMAX = NMAX;
        keys = (Key[]) new Comparable[NMAX + 1];    // make this of length NMAX??
        pq   = new int[NMAX + 1];
        qp   = new int[NMAX + 1];                   // make this of length NMAX??
        for (int i = 0; i <= NMAX; i++) qp[i] = -1;
    }

    /**
     * Is the priority queue empty?
     * @return true if the priority queue is empty; false otherwise
     */
    public boolean isEmpty() {
        return N == 0;
    }

    /**
     * Is i an index on the priority queue?
     * @param i an index
     * @throws java.lang.IndexOutOfBoundsException unless (0 &le; i < NMAX)
     */
    public boolean contains(int i) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        return qp[i] != -1;
    }

    /**
     * Returns the number of keys on the priority queue.
     * @return the number of keys on the priority queue
     */
    public int size() {
        return N;
    }

    /**
     * Associates key with index i.
     * @param i an index
     * @param key the key to associate with index i
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.util.IllegalArgumentException if there already is an item associated with index i
     */
    public void insert(int i, Key key) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (contains(i)) throw new IllegalArgumentException("index is already in the priority queue");
        N++;
        qp[i] = N;
        pq[N] = i;
        keys[i] = key;
        swim(N);
    }

    /**
     * Returns an index associated with a minimum key.
     * @return an index associated with a minimum key
     * @throws java.util.NoSuchElementException if priority queue is empty
     */
    public int minIndex() { 
        if (N == 0) throw new NoSuchElementException("Priority queue underflow");
        return pq[1];        
    }

    /**
     * Returns a minimum key.
     * @return a minimum key
     * @throws java.util.NoSuchElementException if priority queue is empty
     */
    public Key minKey() { 
        if (N == 0) throw new NoSuchElementException("Priority queue underflow");
        return keys[pq[1]];        
    }

    /**
     * Removes a minimum key and returns its associated index.
     * @return an index associated with a minimum key
     * @throws java.util.NoSuchElementException if priority queue is empty
     */
    public int delMin() { 
        if (N == 0) throw new NoSuchElementException("Priority queue underflow");
        int min = pq[1];        
        exch(1, N--); 
        sink(1);
        qp[min] = -1;            // delete
        keys[pq[N+1]] = null;    // to help with garbage collection
        pq[N+1] = -1;            // not needed
        return min; 
    }

    /**
     * Returns the key associated with index i.
     * @param i the index of the key to return
     * @return the key associated with index i
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.util.NoSuchElementException no key is associated with index i
     */
    public Key keyOf(int i) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
        else return keys[i];
    }

    /**
     * Change the key associated with index i to the specified value.
     * @param i the index of the key to change
     * @param key change the key assocated with index i to this key
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @deprecated Replaced by changeKey()
     */
    @Deprecated public void change(int i, Key key) {
        changeKey(i, key);
    }

    /**
     * Change the key associated with index i to the specified value.
     * @param i the index of the key to change
     * @param key change the key assocated with index i to this key
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.util.NoSuchElementException no key is associated with index i
     */
    public void changeKey(int i, Key key) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
        keys[i] = key;
        swim(qp[i]);
        sink(qp[i]);
    }

    /**
     * Decrease the key associated with index i to the specified value.
     * @param i the index of the key to decrease
     * @param key decrease the key assocated with index i to this key
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.lang.IllegalArgumentException if key &ge; key associated with index i
     * @throws java.util.NoSuchElementException no key is associated with index i
     */
    public void decreaseKey(int i, Key key) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
        if (keys[i].compareTo(key) <= 0) throw new IllegalArgumentException("Calling decreaseKey() with given argument would not strictly decrease the key");
        keys[i] = key;
        swim(qp[i]);
    }

    /**
     * Increase the key associated with index i to the specified value.
     * @param i the index of the key to increase
     * @param key increase the key assocated with index i to this key
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.lang.IllegalArgumentException if key &le; key associated with index i
     * @throws java.util.NoSuchElementException no key is associated with index i
     */
    public void increaseKey(int i, Key key) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
        if (keys[i].compareTo(key) >= 0) throw new IllegalArgumentException("Calling increaseKey() with given argument would not strictly increase the key");
        keys[i] = key;
        sink(qp[i]);
    }

    /**
     * Remove the key associated with index i.
     * @param i the index of the key to remove
     * @throws java.lang.IndexOutOfBoundsException unless 0 &le; i < NMAX
     * @throws java.util.NoSuchElementException no key is associated with index i
     */
    public void delete(int i) {
        if (i < 0 || i >= NMAX) throw new IndexOutOfBoundsException();
        if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
        int index = qp[i];
        exch(index, N--);
        swim(index);
        sink(index);
        keys[i] = null;
        qp[i] = -1;
    }


   /**************************************************************
    * General helper functions
    **************************************************************/
    private boolean greater(int i, int j) {
        return keys[pq[i]].compareTo(keys[pq[j]]) > 0;
    }

    private void exch(int i, int j) {
        int swap = pq[i]; pq[i] = pq[j]; pq[j] = swap;
        qp[pq[i]] = i; qp[pq[j]] = j;
    }


   /**************************************************************
    * Heap helper functions
    **************************************************************/
    private void swim(int k)  {
        while (k > 1 && greater(k/2, k)) {
            exch(k, k/2);
            k = k/2;
        }
    }

    private void sink(int k) {
        while (2*k <= N) {
            int j = 2*k;
            if (j < N && greater(j, j+1)) j++;
            if (!greater(k, j)) break;
            exch(k, j);
            k = j;
        }
    }


   /***********************************************************************
    * Iterators
    **********************************************************************/

    /**
     * Returns an iterator that iterates over the keys on the
     * priority queue in ascending order.
     * The iterator doesn't implement <tt>remove()</tt> since it's optional.
     * @return an iterator that iterates over the keys in ascending order
     */
    public Iterator<Integer> iterator() { return new HeapIterator(); }

    private class HeapIterator implements Iterator<Integer> {
        // create a new pq
        private IndexMinPQ<Key> copy;

        // add all elements to copy of heap
        // takes linear time since already in heap order so no keys move
        public HeapIterator() {
            copy = new IndexMinPQ<Key>(pq.length - 1);
            for (int i = 1; i <= N; i++)
                copy.insert(pq[i], keys[pq[i]]);
        }

        public boolean hasNext()  { return !copy.isEmpty();                     }
        public void remove()      { throw new UnsupportedOperationException();  }

        public Integer next() {
            if (!hasNext()) throw new NoSuchElementException();
            return copy.delMin();
        }
    }


    /**
     * Unit tests the <tt>IndexMinPQ</tt> data type.
     */
    public static void main(String[] args) {
        // insert a bunch of strings
        String[] strings = { "it", "was", "the", "best", "of", "times", "it", "was", "the", "worst" };

        IndexMinPQ<String> pq = new IndexMinPQ<String>(strings.length);
        for (int i = 0; i < strings.length; i++) {
            pq.insert(i, strings[i]);
        }

        // delete and print each key
        while (!pq.isEmpty()) {
            int i = pq.delMin();
            StdOut.println(i + " " + strings[i]);
        }
        StdOut.println();

        // reinsert the same strings
        for (int i = 0; i < strings.length; i++) {
            pq.insert(i, strings[i]);
        }

        // print each key using the iterator
        for (int i : pq) {
            StdOut.println(i + " " + strings[i]);
        }
        while (!pq.isEmpty()) {
            pq.delMin();
        }

    }
}
