/*************************************************************************
 *  Compilation:  javac DijkstraAllPairsSP.java
 *  Dependencies: EdgeWeightedDigraph.java Dijkstra.java
 *
 *  Dijkstra's algorithm run from each vertex. 
 *  Takes time proportional to E V log V and space proportional to EV.
 *
 *************************************************************************/

/**
 *  The <tt>DijkstraAllPairsSP</tt> class represents a data type for solving the
 *  all-pairs shortest paths problem in edge-weighted digraphs
 *  where the edge weights are nonnegative.
 *  <p>
 *  This implementation runs Dijkstra's algorithm from each vertex.
 *  The constructor takes time proportional to <em>V</em> (<em>E</em> log <em>V</em>)
 *  and uses space proprtional to <em>V</em><sup>2</sup>,
 *  where <em>V</em> is the number of vertices and <em>E</em> is the number of edges.
 *  Afterwards, the <tt>dist()</tt> and <tt>hasPath()</tt> methods take
 *  constant time and the <tt>path()</tt> method takes time proportional to the
 *  number of edges in the shortest path returned.
 *  <p>
 *  For additional documentation, see <a href="/algs4/44sp">Section 4.4</a> of
 *  <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class DijkstraAllPairsSP {
    private DijkstraSP[] all;

    /**
     * Computes a shortest paths tree from each vertex to to every other vertex in
     * the edge-weighted digraph <tt>G</tt>.
     * @param G the edge-weighted digraph
     * @throws IllegalArgumentException if an edge weight is negative
     * @throws IllegalArgumentException unless 0 &le; <tt>s</tt> &le; <tt>V</tt> - 1
     */
    public DijkstraAllPairsSP(EdgeWeightedDigraph G) {
        all  = new DijkstraSP[G.V()];
        for (int v = 0; v < G.V(); v++)
            all[v] = new DijkstraSP(G, v);
    }

    /**
     * Returns a shortest path from vertex <tt>s</tt> to vertex <tt>t</tt>.
     * @param s the source vertex
     * @param t the destination vertex
     * @return a shortest path from vertex <tt>s</tt> to vertex <tt>t</tt>
     *    as an iterable of edges, and <tt>null</tt> if no such path
     */
    public Iterable<DirectedEdge> path(int s, int t) {
        return all[s].pathTo(t);
    }

    /**
     * Is there a path from the vertex <tt>s</tt> to vertex <tt>t</tt>?
     * @param s the source vertex
     * @param t the destination vertex
     * @return <tt>true</tt> if there is a path from vertex <tt>s</tt> 
     *    to vertex <tt>t</tt>, and <tt>false</tt> otherwise
     */
    public boolean hasPath(int s, int t) {
        return dist(s, t) < Double.POSITIVE_INFINITY;
    }

    /**
     * Returns the length of a shortest path from vertex <tt>s</tt> to vertex <tt>t</tt>.
     * @param s the source vertex
     * @param t the destination vertex
     * @return the length of a shortest path from vertex <tt>s</tt> to vertex <tt>t</tt>;
     *    <tt>Double.POSITIVE_INFINITY</tt> if no such path
     */
    public double dist(int s, int t) {
        return all[s].distTo(t);
    }
}
