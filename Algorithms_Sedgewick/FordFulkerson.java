/*************************************************************************
 *  Compilation:  javac FordFulkerson.java
 *  Execution:    java FordFulkerson V E
 *  Dependencies: FlowNetwork.java FlowEdge.java Queue.java
 *
 *  Ford-Fulkerson algorithm for computing a max flow and 
 *  a min cut using shortest augmenting path rule.
 *
 *********************************************************************/

/**
 *  The <tt>FordFulkerson</tt> class represents a data type for computing a
 *  <em>maximum st-flow</em> and <em>minimum st-cut</em> in a flow
 *  network.
 *  <p>
 *  This implementation uses the <em>Ford-Fulkerson</em> algorithm with
 *  the <em>shortest augmenting path</em> heuristic.
 *  The constructor takes time proportional to <em>E V</em> (<em>E</em> + <em>V</em>)
 *  in the worst case and extra space (not including the network)
 *  proportional to <em>V</em>, where <em>V</em> is the number of vertices
 *  and <em>E</em> is the number of edges. In practice, the algorithm will
 *  run much faster.
 *  Afterwards, the <tt>inCut()</tt> and <tt>value()</tt> methods take
 *  constant time.
 *  <p>
 *  If the capacities and initial flow values are all integers, then this
 *  implementation guarantees to compute an integer-valued maximum flow.
 *  If the capacities and floating-point numbers, then floating-point
 *  roundoff error can accumulate.
 *  <p>
 *  For additional documentation, see <a href="http://algs4.cs.princeton.edu/64maxflow">Section 6.4</a>
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class FordFulkerson {
    private boolean[] marked;     // marked[v] = true iff s->v path in residual graph
    private FlowEdge[] edgeTo;    // edgeTo[v] = last edge on shortest residual s->v path
    private double value;         // current value of max flow
  
    /**
     * Compute a maximum flow and minimum cut in the network <tt>G</tt>
     * from vertex <tt>s</tt> to vertex <tt>t</tt>.
     * @param G the flow network
     * @param s the source vertex
     * @param t the sink vertex
     * @throws IndexOutOfBoundsException unless 0 <= s < V
     * @throws IndexOutOfBoundsException unless 0 <= t < V
     * @throws IllegalArgumentException if s = t
     * @throws IllegalArgumentException if initial flow is infeasible
     */
    public FordFulkerson(FlowNetwork G, int s, int t) {
        if (s < 0 || s >= G.V()) {
            throw new IndexOutOfBoundsException("Source s is invalid: " + s);
        }
        if (t < 0 || t >= G.V()) {
            throw new IndexOutOfBoundsException("Sink t is invalid: " + t);
        }
        if (s == t) {
            throw new IllegalArgumentException("Source equals sink");
        }
        value = excess(G, t);
        if (!isFeasible(G, s, t)) {
            throw new IllegalArgumentException("Initial flow is infeasible");
        }

        // while there exists an augmenting path, use it
        while (hasAugmentingPath(G, s, t)) {

            // compute bottleneck capacity
            double bottle = Double.POSITIVE_INFINITY;
            for (int v = t; v != s; v = edgeTo[v].other(v)) {
                bottle = Math.min(bottle, edgeTo[v].residualCapacityTo(v));
            }

            // augment flow
            for (int v = t; v != s; v = edgeTo[v].other(v)) {
                edgeTo[v].addResidualFlowTo(v, bottle); 
            }

            value += bottle;
        }

        // check optimality conditions
        assert check(G, s, t);
    }

    /**
     * Returns the value of the maximum flow.
     * @return the value of the maximum flow
     */
    public double value()  {
        return value;
    }

    // is v in the s side of the min s-t cut?
    /**
     * Is vertex <tt>v</tt> on the <tt>s</tt> side of the minimum st-cut?
     * @return <tt>true</tt> if vertex <tt>v</tt> is on the <tt>s</tt> side of the micut,
     *    and <tt>false</tt> if vertex <tt>v</tt> is on the <tt>t</tt> side.
     * @throws IndexOutOfBoundsException unless 0 <= v < V
     */
    public boolean inCut(int v)  {
        int V = marked.length;
        if (v < 0 || v >= V)
            throw new IndexOutOfBoundsException("vertex " + v + " is not between 0 and " + (V-1));
        return marked[v];
    }


    // is there an augmenting path? 
    // if so, upon termination edgeTo[] will contain a parent-link representation of such a path
    private boolean hasAugmentingPath(FlowNetwork G, int s, int t) {
        edgeTo = new FlowEdge[G.V()];
        marked = new boolean[G.V()];

        // breadth-first search
        Queue<Integer> q = new Queue<Integer>();
        q.enqueue(s);
        marked[s] = true;
        while (!q.isEmpty()) {
            int v = q.dequeue();

            for (FlowEdge e : G.adj(v)) {
                int w = e.other(v);

                // if residual capacity from v to w
                if (e.residualCapacityTo(w) > 0) {
                    if (!marked[w]) {
                        edgeTo[w] = e;
                        marked[w] = true;
                        q.enqueue(w);
                    }
                }
            }
        }

        // is there an augmenting path?
        return marked[t];
    }



    // return excess flow at vertex v
    private double excess(FlowNetwork G, int v) {
        double excess = 0.0;
        for (FlowEdge e : G.adj(v)) {
            if (v == e.from()) excess -= e.flow();
            else               excess += e.flow();
        }
        return excess;
    }

    // return excess flow at vertex v
    private boolean isFeasible(FlowNetwork G, int s, int t) {
        double EPSILON = 1E-11;

        // check that capacity constraints are satisfied
        for (int v = 0; v < G.V(); v++) {
            for (FlowEdge e : G.adj(v)) {
                if (e.flow() < -EPSILON || e.flow() > e.capacity() + EPSILON) {
                    System.err.println("Edge does not satisfy capacity constraints: " + e);
                    return false;
                }
            }
        }

        // check that net flow into a vertex equals zero, except at source and sink
        if (Math.abs(value + excess(G, s)) > EPSILON) {
            System.err.println("Excess at source = " + excess(G, s));
            System.err.println("Max flow         = " + value);
            return false;
        }
        if (Math.abs(value - excess(G, t)) > EPSILON) {
            System.err.println("Excess at sink   = " + excess(G, t));
            System.err.println("Max flow         = " + value);
            return false;
        }
        for (int v = 0; v < G.V(); v++) {
            if (v == s || v == t) continue;
            else if (Math.abs(excess(G, v)) > EPSILON) {
                System.err.println("Net flow out of " + v + " doesn't equal zero");
                return false;
            }
        }
        return true;
    }



    // check optimality conditions
    private boolean check(FlowNetwork G, int s, int t) {

        // check that flow is feasible
        if (!isFeasible(G, s, t)) {
            System.err.println("Flow is infeasible");
            return false;
        }

        // check that s is on the source side of min cut and that t is not on source side
        if (!inCut(s)) {
            System.err.println("source " + s + " is not on source side of min cut");
            return false;
        }
        if (inCut(t)) {
            System.err.println("sink " + t + " is on source side of min cut");
            return false;
        }

        // check that value of min cut = value of max flow
        double mincutValue = 0.0;
        for (int v = 0; v < G.V(); v++) {
            for (FlowEdge e : G.adj(v)) {
                if ((v == e.from()) && inCut(e.from()) && !inCut(e.to()))
                    mincutValue += e.capacity();
            }
        }

        double EPSILON = 1E-11;
        if (Math.abs(mincutValue - value) > EPSILON) {
            System.err.println("Max flow value = " + value + ", min cut value = " + mincutValue);
            return false;
        }

        return true;
    }


    /**
     * Unit tests the <tt>FordFulkerson</tt> data type.
     */
    public static void main(String[] args) {

        // create flow network with V vertices and E edges
        int V = Integer.parseInt(args[0]);
        int E = Integer.parseInt(args[1]);
        int s = 0, t = V-1;
        FlowNetwork G = new FlowNetwork(V, E);
        StdOut.println(G);

        // compute maximum flow and minimum cut
        FordFulkerson maxflow = new FordFulkerson(G, s, t);
        StdOut.println("Max flow from " + s + " to " + t);
        for (int v = 0; v < G.V(); v++) {
            for (FlowEdge e : G.adj(v)) {
                if ((v == e.from()) && e.flow() > 0)
                    StdOut.println("   " + e);
            }
        }

        // print min-cut
        StdOut.print("Min cut: ");
        for (int v = 0; v < G.V(); v++) {
            if (maxflow.inCut(v)) StdOut.print(v + " ");
        }
        StdOut.println();

        StdOut.println("Max flow value = " +  maxflow.value());
    }

}
