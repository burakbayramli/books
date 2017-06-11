"""
Determines the triadic census of a graph
"""
import networkx as nx
__author__ = "\n".join(['Max Tsvetovat (maksim@tsvetovat.org)', 'revised from code by Alex Levenson (alex@isnontinvain.com) and Diederik van Liere (diederik.vanliere@rotman.utoronto.ca)'])

#    (C) Maksim Tsvetovat, 2011

#    Revised from triadic.py by
#    (C) Reya Group: http://www.reyagroup.com
#    Alex Levenson (alex@isnotinvain.com)
#    Diederik van Liere (diederik.vanliere@rotman.utoronto.ca)
#    BSD license.

__all__ = ["triadic_census"]

triad_names = ("003", "012", "102", "021D","021U", "021C", "111D", "111U",
               "030T", "030C", "201", "120D","120U", "120C", "210", "300")
tricodes = (1, 2, 2, 3, 2, 4, 6, 8, 2, 6, 5, 7, 3, 8, 7, 11, 2, 6, 4, 8, 5, 9,
            9, 13, 6, 10, 9, 14, 7, 14, 12, 15, 2, 5, 6, 7, 6, 9, 10, 14, 4, 9,
            9, 12, 8, 13, 14, 15, 3, 7, 8, 11, 7, 12, 14, 15, 8, 14, 13, 15, 
            11, 15, 15, 16)
tricode_to_name = dict((i,triad_names[tricodes[i] - 1])
                       for i in range(len(tricodes)))

def triad_graphs(type=None):
    # Returns dictionary mapping triad names to triad graphs
    def abc_graph():
        g=nx.DiGraph()
        g.add_nodes_from('abc')
        return g
    tg = dict((n, abc_graph()) for n in triad_names)
    tg['012'].add_edges_from([('a','b')])
    tg['102'].add_edges_from([('a','b'),('b','a')])
    tg['102'].add_edges_from([('a','b'),('b','a')])
    tg['021D'].add_edges_from([('b','a'),('b','c')])
    tg['021U'].add_edges_from([('a','b'),('c','b')])
    tg['021C'].add_edges_from([('a','b'),('b','c')])
    tg['111D'].add_edges_from([('a','c'),('c','a'),('b','c')])
    tg['111U'].add_edges_from([('a','c'),('c','a'),('c','b')])
    tg['030T'].add_edges_from([('a','b'),('c','b'),('a''c')])
    tg['030C'].add_edges_from([('b','a'),('c','b'),('a','c')])
    tg['201'].add_edges_from([('a','b'),('b','a'),('a','c'),('c','a')])
    tg['120D'].add_edges_from([('b','c'),('b','a'),('a','c'),('c','a')])
    tg['120C'].add_edges_from([('a','b'),('b','c'),('a','c'),('c','a')])
    tg['120U'].add_edges_from([('a','b'),('c','b'),('a','c'),('c','a')])
    tg['210'].add_edges_from([('a','b'),('b','c'),('c','b'),('a','c'),
                               ('c','a')])
    tg['300'].add_edges_from([('a','b'),('b','a'),('b','c'),('c','b'),
                               ('a','c'),('c','a')])
    return tg

def _tricode(G, v, u, w):
    """This is some fancy magic that comes from Batagelj and Mrvar's paper.
    It treats each link between v,u,w as a bit in the binary representation 
    of an integer. This number then is mapped to one of the 16 triad types.
    """
    combos = ((v, u, 1), (u, v, 2), (v, w, 4), 
              (w, v, 8), (u, w, 16), (w, u, 32))
    return sum(x for u,v,x in combos if v in G[u])

def triadic_census(G):
    """
    Determines the triadic census of a digraph

    Triadic census is a count of how many of the 16 possible types of 
    triad are present in a directed graph.

    Parameters
    ----------
    G : digraph
        A NetworkX DiGraph 
        If a non-directed graph is passed in, it will be converted to a symmetric directed graph

    Returns
    -------
    census : dict
        Dictionary with triad names as keys and number of occurances as values
    
    node_census : dict of dicts
        Dictionary with node IDs as keys, and a triadic census for each node as a value
        The value is a dict with triad names as keys and number of occurances as values

    Notes
    -----
    This algorithm has complexity O(m) where m is the number of edges in the
    graph.

    Refrences
    ---------
    .. [1] Vladimir Batagelj and Andrej Mrvar,  A subquadratic triad 
        census algorithm for large sparse networks with small maximum degree,
        University of Ljubljana,
        http://vlado.fmf.uni-lj.si/pub/networks/doc/triads/triads.pdf
    """
    if not G.is_directed():
        G=nx.DiGraph(G) # convert an undirected graph to a directed graph
        #raise nx.NetworkXError("Not defined for undirected graphs.")
      
    # initialze the count to zero
    census = dict((name, 0) for name in triad_names)
    node_census = dict ((v, dict((name, 0) for name in triad_names)) for v in G.nodes())
    n = len(G)
    m = dict(zip(G, range(n)))
    for v in G:
        vnbrs = set(G.pred[v]) | set(G.succ[v])
        for u in vnbrs:
            if m[u] <= m[v]:
                continue
            neighbors = (vnbrs | set(G.succ[u]) | set(G.pred[u])) - set([u,v])
            # calculate dyadic triads instead of counting them
            if v in G[u] and u in G[v]:
                census["102"] += n - len(neighbors) - 2
                node_census[v]["102"] += n - len(neighbors) - 2
            else:
                census["012"] += n - len(neighbors) - 2
                node_census[v]["012"] += n - len(neighbors) - 2
            # count connected triads
            for w in neighbors:
                if (m[u] < m[w]) or  (m[v] < m[w] and 
                                      m[w] < m[u] and 
                                      not v in G.pred[w] and 
                                      not v in G.succ[w]):
                    code = _tricode(G, v, u, w)
                    census[tricode_to_name[code]] += 1
                    node_census[v][tricode_to_name[code]] += 1

    # null triads = total number of possible triads - all found triads        
    census["003"] = ((n * (n - 1) * (n - 2)) / 6) - sum(census.values())
    return census, node_census

