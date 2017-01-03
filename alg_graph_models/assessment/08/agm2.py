"""
There is scope for research here. The usual approach in the literature
is 1) convert the problem to finding a minimal separator in 'ug' the
moralised version of the minimal ancestral graph containing both
variables and 2) use maximum flow methods to find a minimal separator in ug.

There are papers by Tian and Pearl and by de Campos and Acid which are
helpful.

Using eg Menger's theorem we have that the size of a minimal
separating set between (non-adjacent) a and b is equal to the maximum
number of internally disjoint paths between a and b. Rather than find
these paths directly (as de Campos and Acid), we exploit the 'edge'
version of Menger's theorem: the minimal separating set of *edges*
equals the maximum number of *edge-disjoint* paths.

This approach is taken since we can then directly apply the Edmonds
Karp algorithm to find the minimal separating set of edges. This is a
'maximal flow' algorithm. This requires the construction of a graph
where the vertices of the original graph become edges. The following
does not employ the shortcut which Acid and de Campos use.

It is not enough for students to implement existing algorithms
(although this is not too easy given the way they are presented in the
literature!). An argument that the implementation is correct is needed also.

"""

from gPy.Graphs import DiGraph

def maximum_flow(dg,a,b):
    """Edmonds-Karp algorithm. Complexity is O(VE^2)
    """

    residual_network = dg.copy()
        
    # a flow is represented by a digraph,
    # initally empty, representing flow of zero along
    # each edge
    flow = DiGraph(residual_network.vertices())

    new_path = shortest_path(residual_network,a,b)
    while new_path:
        # iterate through each arrow  ..
        for i, frm in enumerate(new_path[:-1]):
            to = new_path[i+1]
            # .. and adjust the flow
            if flow.is_parent(to,frm):
                flow.discard_arrow(to,frm)
            else:
                flow.add_arrow(frm,to)

            # reverse arrow
            residual_network.discard_arrow(frm,to)
            residual_network.add_arrow(to,frm)
            

        new_path = shortest_path(residual_network,a,b)
    # need to use residual_network to get bottlenecks.
    return flow, residual_network

def shortest_path(dg,a,b):
    """Simple breadth-first search
    """
    mother = {a:None}
    openlist = [a]
    path_found = False
    while openlist and not path_found:
        # take from the start of the list  ...
        node = openlist.pop(0)
        for newone in dg.children(node).difference(mother):
            mother[newone] = node
            if newone == b:
                path_found = True
                break
            else:
                # ... and add to the end of the list
                openlist.append(newone)
    if path_found:
        path = [b]
        while path[-1] != a:
            path.append(mother[path[-1]])
        path.reverse()
        return path
    else:
        return None
    

def vertices_to_edges(ug):
    """complexity is O(V+E)
    """
    new_dg = DiGraph()
    for vertex in ug.vertices():
        neg = (vertex,'-')
        pos = (vertex,'+')
        new_dg.add_vertex(neg)
        new_dg.add_vertex(pos)
        new_dg.add_arrow(pos,neg)
    for v1, v2 in ug.lines():
        new_dg.add_arrow((v1,'-'),(v2,'+'))
        new_dg.add_arrow((v2,'-'),(v1,'+'))
    return new_dg
    

def find_minimal_separator(adg,a,b):
    # create appropriate undirected graph ...
    ug = adg.ancestral_adg(frozenset([a])|frozenset([b])).moralise()

    # short cut
    if ug.is_neighbour(a,b):
        return None

    # convert vertices of ug into edges of a directed graph
    # in the style of Acid and de Campos
    dg = vertices_to_edges(ug)


    s = (a,'-')
    t = (b, '+')

    # get maximum flow between s and t
    # this will contain the maximum number of edge disjoint paths
    # from s to t 
    max_flow, residual_network =  maximum_flow(dg,s,t)

    # get all the paths from s to t in max_flow
    flow_paths = []
    for child in max_flow.children(s):
        flow_path = [(s,child)]
        while child != t:
            new_child = max_flow.child(child)
            flow_path.append((child,new_child))
            child = new_child
        flow_paths.append(flow_path)

    # Each flow path contains at least one critical edge
    # which if removed stops the flow from s to t along that flow path.
    # To find such an edge, first remove the other flow paths (if any)
    # Then just check the edges in current flow_path. If the removal
    # of an edge would interrupt the flow, then it is a critical edge
    # and the corresponding vertex is in the minimal separator.
    minimal_separator = []
    for i, flow_path in enumerate(flow_paths):
        fdg = dg.copy()
        for other in flow_paths[:i] + flow_paths[i+1:]:
            for pa, ch in other:
                fdg.discard_arrow(pa,ch)
        for pa, ch in flow_path:
            if pa[0] == ch[0] and pa[1] == '+' and ch[1] == '-':
                if t not in fdg.reachable(frozenset([s]),[pa,ch]):
                    minimal_separator.append(pa[0])
                    break
    return frozenset(minimal_separator)

