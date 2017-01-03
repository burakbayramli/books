"""Graphs

@var _version: Version of this module
@type _version: String
"""

import Tkinter
import random
from Utils import member, emptyset, pretty_str_set, scrolled_frame
from IO import read_twlib, GraphCanvas
from Hypergraphs import Incidence, ReducedGraphicalHypergraph

_version = '$Id: Graphs.py,v 1.2 2008/10/07 09:09:58 jc Exp $'

class GraphError(Exception):
    pass

class CycleError(GraphError):
    pass

class DirectedCycleError(GraphError):
    pass

class ExistingVertexError(GraphError):
    pass

class LineError(GraphError):
    pass

class ArrowError(GraphError):
    pass

class EdgeError(GraphError):
    pass

class ArcError(GraphError):
    pass

class _AbsDiGraph(Incidence):
    """Abstract class containing methods which are appropriate only 
    for graphs where arrows are allowed.
    """

    def add_arrow(self,frm,to):
        """Add an arrow (directed edge) between two
        existing vertices

        Does not affect any lines from C{frm} to C{to}
        @param frm: One of the vertices connected by the line
        @type frm: Any immutable type
        @param to: The other vertex connected by the line
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._ch[frm].add(to)
        self._pa[to].add(frm)

    def add_arrows(self,arrows):
        """Add a collection of arrows to the graph

        @param arrows: The arrows to add
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for arrow in arrows:
            self.add_arrow(arrow[0],arrow[1])

    def put_arrow(self,frm,to):
        """Put an arrow (directed edge) between two
        existing vertices

        Any pre-existing line between C{frm} and C{to} will be deleted. 
        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        @raise DirectedCycleError: If C{self} is an ADG and the
        arrow would create a directed cycle
        """
        self.add_arrow(frm,to)
        self.discard_line(frm,to)

    def put_arrows(self,arrows):
        """Add a collection of arrows to the graph

        Any pre-existing lines will be deleted. 
        @param arrows: The arrows to add
        @type arrows: An iterator over pairs (of vertices).
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        @raise DirectedCycleError: If C{self} is an ADG and one of the
        arrows would create a directed cycle
        """
        for arrow in arrows:
            self.put_arrow(arrow[0],arrow[1])

    def put_family(self,child,parents):
        """Put arrows from all C{parents} to C{child},
        creating vertices when needed

        Any existing lines between parents and children
        will be overwritten
        @param child: The vertex to which all added arrows will point
        @type child: Immutable type
        @param parents: The collection of vertices from which there will be
        arrows pointing to child
        @type parents: An iterator over immutable types
        @raise DirectedCycleError: If C{self} is an ADG and one of the
        arrows would create a directed cycle
        """
        existing_vertices = self.vertices()
        if child not in existing_vertices:
            self.add_vertex(child)
        for parent in parents:
            if parent not in existing_vertices:
                self.add_vertex(parent)
            self.put_arrow(parent,child)

    def remove_arrow(self,frm,to):
        """Remove an arrow between two existing vertices

        @param frm: One of the vertices connected by the possible arrow
        @type frm: Immutable type
        @param to: The other vertex connected by the possible arrow
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        @raise KeyError: If the arrow is not there
        """
        self._pa[to].remove(frm)
        self._ch[frm].remove(to)


class _AbsGraph(Incidence):
    """Abstract class with method suitable for all types of graph
    
    @ivar _pa: Maps each vertex to its parents
    @type _pa: Dictionary
    @ivar _ch: Maps each vertex to its children
    @type _ch: Dictionary
    @ivar _ne: Maps each vertex to its neighbours
    @type _ne: Dictionary
    @ivar _vertex_positions: Maps vertices to canvas co-ordinates
    @type _vertex_positions: Dictionary
    """
    
    def __init__(self,vertices=(),arrows=(),lines=(),vertex_positions=None):
        """Graph initialisation

        @param vertices: The vertices of the graph
        @type vertices: An iterator/sequence over immutable types
        @param arrows: The arrows of the graph
        @type arrows: An iterator/sequence over pairs (of vertices).
        Each pair is a sequence of length 2.
        @param lines: The lines of the graph
        @type lines: An iterator/sequence over pairs (of vertices).
        Each pair is a sequence of length 2.
        @param vertex_positions: A mapping from vertices to canvas
        co-ordinates
        @type vertex_positions: Dictionary
        @raise KeyError: If C{arrows} or C{lines} contains a vertex not included in
        C{vertices}
        @raise ExistingVertexError: If C{vertices} repeats a vertex.
        """
        if vertex_positions is None:
            vertex_positions = {}
        self.reinit(vertices,arrows,lines,vertex_positions)

    def __eq__(self,other):
        """Graphs of different classes are considered equal if they have the same vertices, arrows
        and lines

        @return: Whether the graphs are equal
        @rtype: Boolean
        """
        return (
            self._pa == other._pa and
            self._ne == other._ne
            )

    def __ne__(self,other):
        """Graphs of different classes are considered equal if they
        have the same vertices, arrows  and lines
        
        @return: Whether the graphs are not equal
        @rtype: Boolean
        """
        return (
            self._pa != other._pa or
            self._ne != other._ne
            )

    def __repr__(self):
        """Formal string representation of a graph
        """
        return '%s(%s,%s,%s,%s)' % (
            self.__class__.__name__,
            sorted(self.vertexlist()),
            sorted(self.arrows()),
            sorted(self.lines()),
            self._vertex_positions)

    def __str__(self):
        """Pretty string representation of a graph

        Vertices, arrows and edges are presented in
        sorted order
        @return: Pretty string representation of a graph
        @rtype: String
        """
        out = 'Vertices:\n%s\n' % sorted(self.vertexlist())
        arrows = self.arrows()
        if arrows:
            out += 'Arrows:\n'
            for arrow in sorted(arrows):
                out += '%s -> %s\n' % arrow
        lines = self.lines()
        if lines:
            out += 'Lines:\n'
            for line in sorted(lines):
                out += '%s - %s\n' % line
        return out

    def add_vertex(self,vertex):
        '''Adds a vertex to a graph

        @param vertex: The vertex to add
        @type vertex: Anything immutable
        @raise ExistingVertexError: If vertex already exists
        '''
        if vertex in self._pa:
            raise ExistingVertexError
        for dkt in (self._pa, self._ch, self._ne):
            dkt[vertex] = set()


    def add_vertices(self,vertices):
        """Add a collection of vertices to the graph

        @param vertices: The vertices to add
        @type vertices: An iterator/sequence over immutable types
        """
        for vertex in vertices:
            self.add_vertex(vertex)

    def arbitrary_vertex(self):
        """Returns an arbitrary vertex in the graph

        @return: A vertex in the graph
        @rtype: Anything immutable
        """
        return self._pa.keys()[0]

    def arrows(self):
        """Return a list of arrows

        @return: A list of arrows
        @rtype: List of C{(vertex,child)} tuples
        """
        arrows = []
        for vertex, children in self._ch.items():
            for child in children:
                arrows.append((vertex,child))
        return arrows

    def child(self,vertex):
        """Return an arbitrary child of C{vertex}

        @return: A vertex which is a child of C{vertex} or None
        if it has no children
        @rtype: An immutable type (typically a String)
        """
        for child in self._ch[vertex]:
            return child


    def children(self,vertex):
         """Return the children of vertex
        
        @return: The children of C{vertex}
        @rtype: Set
        """
         return self._ch[vertex]

    def copy(self):
        """Return a copy a graph

        @return: A copy of the graph
        @rtype: Same class as C{self}
        """
        cp = self.__class__()
        for attr, val in vars(self).items():
            if attr == '_vertex_positions':
                setattr(cp,attr,val.copy())
                continue
            setattr(cp,attr,{})
            for vertex, vertices in val.items():
                getattr(cp,attr)[vertex] = vertices.copy()
        return cp

    def delete_vertex(self,vertex):
        """Deletes a vertex from a graph

        @param vertex: The vertex to delete
        @type vertex: Immutable type
        @raise KeyError: If the vertex does not exist
        """
        for dkt in (self._pa, self._ch, self._ne):
            del dkt[vertex]
            for vertices in dkt.values():
                vertices.discard(vertex)

    def diff(self,other):
        pass

    def discard_arrow(self,frm,to):
        """Discard any arrow between two existing vertices

        If there is no such arrow nothing happens
        @param frm: One of the vertices connected by the possible arrow
        @type frm: Immutable type
        @param to: The other vertex connected by the possible arrow
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._pa[to].discard(frm)
        self._ch[frm].discard(to)

    def discard_line(self,frm,to):
        """Discard the line between two existing vertices
        (if it exists)

        If there is no such line nothing happens
        @param frm: One of the vertices connected by the possible arrow
        @type frm: Immutable type
        @param to: The other vertex connected by the possible arrow
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._ne[to].discard(frm)
        self._ne[frm].discard(to)

    def edges(self):
        """Return a list of the edges in the graph

        @return: A list of edges
        @rtype: List
        """
        edges = []
        for dkt in (self._ne, self._ch):
            for vertex, others in dkt.items():
                for other in others:
                    edges.append((vertex,other))
        return edges        

    def extend_paths(self,paths):
        """Iterates over paths which can be produced by adding
        one vertex to a path in C{paths}
                
        Each path is actually a tuple C{path_set,path}
        where C{path_set} is the set of all vertices in the
        path, and C{path} is a list representing the path itself

        @param paths: Paths to extend
        @type paths: List
        @return: Generator of paths
        @rtype: Generator iterator
        """
        for path_set, path in paths:
            last_vertex = path[-1]
            for new_vertex in ((self._ne[last_vertex] |
                                self._ch[last_vertex]) - path_set):
                new_vertex_list = [new_vertex]
                yield (path_set | set(new_vertex_list),
                       path + new_vertex_list)
                       

    def get_gui_graph_kill(self,gui,win=None):
        """Set the graph to match the one displayed on a GUI,
        and perhaps kill the GUI

        @param gui: The GUI
        @type gui: L{GraphCanvas} object
        @param win: Window to kill
        @type win: C{Tkinter} object
        """
        vertices, arrows, lines, vertex_positions = gui.graph()
        try:
            self.reinit(vertices,arrows,lines,vertex_positions)
            if win is not None:
                win.destroy()
        except AttributeError, msg:
            print 'Graph contains an illegal edge', msg

    def gui_edit(self,parent,**config):
        """Edit a graph using a GUI

        @param parent: Parent window for GUI
        @type parent: Suitable Tkinter widget
        @param config: Configuration options for the GUI
        @type config: Various
        """
        gui_main = Tkinter.Frame(parent)
        gui_main.pack()
        canvas = GraphCanvas(self, gui_main, **config)
        canvas.pack()
        bottom = Tkinter.Frame(gui_main)
        bottom.pack()
        for (txt,cmd) in (('Reset', canvas.original_state),
                          ('Delete', canvas.zap),
                          ('Save', lambda gui=canvas: self.get_gui_graph_kill(gui)),
                          ('Done', (lambda gui=canvas,win=gui_main:
                                    self.get_gui_graph_kill(gui,win))),
                          ('Quit', (lambda gui=canvas,win=parent:
                                    self.get_gui_graph_kill(gui,win))),
                          ('Help', self._gui_help)):
            button = Tkinter.Button(bottom,text=txt,command=cmd)
            button.bind('<Return>', lambda event: cmd())
            button.pack(side=Tkinter.LEFT)
        


    def gui_display(self,parent,scrollable=False,**config):
        """Display a graph using a GUI

        @param parent: Parent window for GUI
        @type parent: Suitable Tkinter widget
        @param config: Configuration options for the GUI
        @type config: Various
        @return: The GUI
        @rtype: L{GraphCanvas}
        """
        if scrollable:
            return self.gui_display(scrolled_frame(parent),False,width=10000,height=1000,**config)
        gui_main = GraphCanvas(self, parent, edit=False, **config)
        gui_main.pack()
        return gui_main
        

    def is_neighbour(self,vertex1,vertex2):
        """Return whether C{vertex1} is a neighbour of C{vertex2}

        @param vertex1: Vertex
        @type vertex1: Immutable
        @param vertex2: Vertex
        @type vertex2: Immutable
        @raise KeyError: If C{vertex1} is not a vertex
        """
        return vertex2 in self._ne[vertex1]

    def is_parent(self,parent,child):
        """Return whether C{parent} is a parent of C{child}

        @param parent: Potential parent vertex
        @type parent: Immutable
        @param child: Potential child vertex
        @type child: Immutable
        @return: Whether C{parent} is a parent of C{child}
        @rtype: Boolean
        @raise KeyError: If C{child} is not a vertex
        """
        return parent in self._pa[child]

        

    def lines(self):
        """Returns an ordered list of lines in the graph

        @return: An ordered list of lines in the graph
        @rtype: List
        """
        lines = []
        vertices = set(self._ne.keys())
        while vertices:
            vertex1 = vertices.pop()
            for vertex2 in self._ne[vertex1] & vertices:
                if vertex1 < vertex2:
                    lines.append((vertex1,vertex2))
                else:
                    lines.append((vertex2,vertex1))
        return lines

    def moralise(self):
        """Return a moralised version of the graph

        All parents are married, and directions are dropped.
        The moral graph will share its vertex co-ordinates, if any, with C{self}
        @return: The moralised graph
        @rtype: L{UGraph} object
        """
        markov = UGraph(self.vertexlist(),lines=self.lines(),vertex_positions=self._vertex_positions)
        for child, parents in self._pa.items():
            for parent in parents:
                markov.put_line(child,parent)
            markov.complete(parents)
        return markov


    def neighbours(self,vertex):
        """Return the set (not a copy) of neighbours of a vertex

        Neighbours are connected by undirected edges (lines)
        @return: Neighbours of C{vertex}
        @rtype: Set
        """
        return self._ne[vertex]

    def orphanlist(self):
        """Return a list containing all nodes with no parents

        @return: Nodes with no parents
        @rtype: List
        """
        orphanlist = []
        for child, parents in self._pa.items():
            if not parents:
                orphanlist.append(child)
        return orphanlist

    def parent(self,vertex):
        """Return an arbitrary parent of vertex
        
        @return: A vertex which is a parent of C{vertex} or None
        if it has no parents
        @rtype: An immutable type (typically a String)
        """
        for parent in self._pa[vertex]:
            return parent

    def num_parents(self,vertex):
        """Return the number of parents of vertex
        
        @return: The number of parents of C{vertex}
        @rtype: Integer
        @raise KeyError: If C{vertex} is not in the graph.
        """
        return len(self._pa[vertex])

    def num_parentsets(self):
        """Iterate over the number of parents of vertices

        (In no particular order)
        """
        for ps in self._pa.values():
            yield len(ps)

    def parents(self,vertex):
        """Return (a copy of) the parents of vertex
        
        @return: The parents of C{vertex}
        @rtype: Set
        @raise KeyError: If C{vertex} is not in the graph.
        """
        return self._pa[vertex].copy()

    def parentsets(self):
        """Iterate over (copies of) all parent sets
        """
        for ps in self._pa.values():
            yield ps.copy()

    def paths(self,vertex):
        """Iterates over paths starting with C{vertex}

        Each path is actually a tuple C{path_set,path}
        where C{path_set} is the set of all vertices in the
        path, and C{path} is a list representing the path itself

        @param vertex: Vertex from which all paths start
        @type vertex: Immutable
        @return: Generator of paths
        @rtype: Generator iterator
        """
        path = (set([vertex]),[vertex])
        yield path
        paths = [path]
        while paths:
            new_paths = []
            for new_path in self.extend_paths(paths):
                new_paths.append(new_path)
                yield new_path
            paths = new_paths

    def put_vertex(self,vertex):
        '''Adds a vertex to a graph or does nothing if it already exists

        @param vertex: The vertex to add
        @type vertex: Immutable type
        '''
        if vertex not in self._pa:
            for dkt in (self._pa, self._ch, self._ne):
                dkt[vertex] = set()


    def reinit(self,vertices,arrows,lines,vertex_positions):
        """(Re-)initialise a graph

        Pre-existing state will be deleted.
        @param vertices: The vertices of the graph
        @type vertices: An iterator/sequence over immutable types
        @param arrows: The arrows of the graph
        @type arrows: An iterator/sequence over pairs (of vertices).
        Each pair is a sequence of length 2.
        @param lines: The lines of the graph
        @type lines: An iterator/sequence over pairs (of vertices).
        Each pair is a sequence of length 2.
        @param vertex_positions: A mapping from vertices to canvas
        co-ordinates
        @type vertex_positions: Dictionary
        @raise KeyError: If C{arrows} or C{lines} contains a vertex not included in
        C{vertices}
        """
        self._pa = {}
        self._ch = {}
        self._ne = {}
        self.add_vertices(vertices)
        if arrows:
            self.add_arrows(arrows)
        if lines:
            self.add_lines(lines)
        self._vertex_positions = vertex_positions

    def remove_vertex(self,vertex):
        """Remove a vertex

        @param vertex: Vertex to be removed
        @type vertex: Immutable
        @raise KeyError: If C{vertex} is not in the graph
        """
        for parent in self._pa[vertex]:
            self._ch[parent].remove(vertex)
        for child in self._ch[vertex]:
            self._pa[child].remove(vertex)
        for nbr in self._ne[vertex]:
            self._ne[nbr].remove(vertex)
        del self._pa[vertex]
        del self._ch[vertex]
        del self._ne[vertex]
        try:
            del self._vertex_positions[vertex]
        except KeyError:
            pass

    def set_vertex_positions(self,coords):
        """
        Position each vertex according to the co-ordinates in C{coords}

        @param coords: A dictionary mapping some (maybe all) vertices to a co-ordinate for display
        @type: Dictionary
        """
        self._vertex_positions.update(coords)

    def shd(self, b):
        """Structural Hamming distance to graph b from C{self}

        +1 for a missing edge (directed or not)
        +1 for an incorrectly oriented edge
        +1 for an extra edge (directed or not)

        Only works on simple graphs (those without multiple connections between vertices)

        @param b: Graph to compare to C{self}
        @type b: L{Graph}
        @return: Structural Hamming distance to graph b from C{self}
        @rtype: Int
        """
        if self.vertices() != b.vertices():
            raise ValueError('Graphs must have the same vertex sets')

        d = 0
        vertices = self._pa.keys()
        for i, v in enumerate(vertices):
            for w in vertices[i+1:]:
                if w in self._pa[v]:
                    if w not in b._pa[v]:
                        d += 1
                elif w in self._ch[v]:
                    if w not in b._ch[v]:
                        d += 1
                elif w in self._ne[v]:
                    if w not in b._ne[v]:
                        d += 1
                elif (w in b._pa[v]) or (w in b._ch[v]) or (w in b._ne[v]):
                    d += 1
        return d

        
        

    def is_subset_of_parents(self,child,vertices):
        """Return whether C{vertices} is a subset of the parents of C{child}
        
        @param child: Child vertex
        @type parent: Immutable
        @param vertices: Set of vertices
        @type vertices: Iterable
        @return: Whether C{vertices} is a subset of C{child}'s parents
        @rtype: Boolean
        @raise KeyError: If C{child} is not a vertex
        """

        return self._pa[child].issuperset(vertices)


    def is_superset_of_parents(self,child,vertices):
        """Return whether C{vertices} is a superset of the parents of C{child}

        @param child: Child vertex
        @type parent: Immutable
        @param vertices: Set of vertices
        @type vertices: Iterable
        @return: Whether C{vertices} is a superset of C{child}'s parents
        @rtype: Boolean
        @raise KeyError: If C{child} is not a vertex
        """

        return self._pa[child].issubset(vertices)
        
    def vertex_positions(self):
        """
        Return a dictionary mapping each vertex to a co-ordinate for display

        @return: A dictionary mapping some (maybe all) vertices to a co-ordinate for display
        @rtype: Dictionary
        """
        return self._vertex_positions.copy()

    def vertexlist(self):
        """Returns a list of the vertices in the graph

        @return: A list of the vertices in the graph
        @rtype: List
        """
        return self._pa.keys()

    def vertices(self):
        """Returns the set of vertices in the graph

        Can be altered without affecting C{self}
        @return: The set of vertices in the graph
        @rtype: Set
        """
        return set(self._pa)

    def _gui_help(self):
        """Display help for editing graphs in a top-level window
        """
        top = Tkinter.Toplevel()
        top.title('Graph editing help')
        Tkinter.Label(top,text=self._edit_help_msg,justify=Tkinter.LEFT).pack()

    _edit_help_msg = """
    Select an object with the left mouse button.
    Clicking on a node with the right button will draw a
    line to the selected node (if any).
    Clicking on a node with the middle button will draw an
    arrow to the selected node (if any).

    You will be prevented from saving a graph with an illegal edge
    (e.g. an arrow in an undirected graph).
    
    'Reset': Return the canvas to its original state
    'Delete': Delete the selected object (node or edge)
    'Save': Save the graph, but do not remove the canvas
    'Done': Save the graph removing canvas and these buttons, but leaving the parent
    'Quit': Save the graph and destroy the parent window
    'Help': Generate this message
    """



class _AbsMixedGraph(Incidence):
    """Abstract class containing methods which are appropriate only for
    graphs where both lines and arrows are allowed.
    """

    def put_edge(self,frm,to):
        """Put an edge (in the mathematical sense)

        @param frm: One of the vertices connected by the edge
        @type frm: Immutable type
        @param to: The other vertex connected by the edge
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        """
        if frm in self._ch[to]:
            self.put_line(frm,to)
        else:
            self.put_arrow(frm,to)




class _AbsUGraph(Incidence):
    """Abstract class containing methods which are appropriate only for
    graphs where lines are allowed.

    @cvar _edit_help_msg: Help message for editing graphs
    @type _edit_help_msg: String
    """

    def add_clique(self,vertices):
        """Add a clique with the specified vertices to the graph

        Vertices may include new vertices. All vertices will be connected
        by undirected edges.
        @param vertices: Vertices to pairwise connect
        @type vertices: Iterable
        """
        for vertex in vertices:
            try:
                self.add_vertex(vertex)
            except ExistingVertexError:
                pass
        self.complete(vertices)


    def add_line(self,frm,to):
        """Add a line (undirected edge) between two
        existing vertices

        @param frm: One of the vertices connected by the line
        @type frm: Any immutable type
        @param to: The other vertex connected by the line
        @type to: Any immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._ne[frm].add(to)
        self._ne[to].add(frm)

    def add_lines(self,lines):
        """Add a collection of lines to the graph

        @param lines: The lines to add
        @type lines: An iterator over pairs of vertices.
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for line in lines:
            self.add_line(line[0],line[1])

    def complete(self,vertices):
        '''Pairwise connects all C{vertices} by undirected edges

        Any existing directed edges are removed
        @param vertices: Vertices to pairwise connect
        @type vertices: Iterable
        @raise KeyError: If a vertex does not exist
        '''
        if not vertices:
            return
        vs = list(vertices)
        vertex1 = vs.pop()
        while vs:
            for vertex2 in vs:
                    self.put_line(vertex1,vertex2)
            vertex1 = vs.pop()

    def put_line(self,frm,to):
        """Put a line (undirected edge) between two
        existing vertices

        Any pre-existing arrow between C{frm} and C{to} in either
        direction will be deleted. 
        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._ne[frm].add(to)
        self._ne[to].add(frm)
        self.discard_arrow(frm,to)
        self.discard_arrow(to,frm)

    def put_lines(self,lines):
        """Add a collection of lines to the graph

        Any pre-existing arrows in either
        direction will be deleted. 
        @param lines: The lines to add
        @type lines: An iterator over pairs of vertices.
        Each pair is a sequence of length 2
        @raise KeyError: If a vertex does not exist
        """
        for line in lines:
            self.put_line(line[0],line[1])

    def remove_line(self,frm,to):
        """Remove a line from the graph

        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        @raise KeyError: If the line is not there
        """
        self._ne[frm].remove(to)
        self._ne[to].remove(frm)


class Graph(_AbsGraph,_AbsDiGraph,_AbsUGraph,_AbsMixedGraph):
    """Unrestricted graphs"""
    pass
        

class DiGraph(_AbsGraph,_AbsDiGraph):
    """Graphs with only directed edges (i.e. arrows)
    """

    def __init__(self,vertices=(),arrows=(),vertex_positions=None):
        _AbsGraph.__init__(self,vertices,arrows,(),vertex_positions)
        

    def __repr__(self):
        return '%s(%s,%s,%s)' % (
            self.__class__.__name__,
            sorted(self.vertexlist()),
            sorted(self.arrows()),
            self._vertex_positions)

    def ancestors(self,vertex):
        """Returns the set of all ancestors of C{vertex}.
        @param vertex: A vertex in the graph
        @type vertex: Immutable
        @return: ancestors of C{vertex}
        @rtype: frozenset of vertices
        """
        ancestors = set()
        self._ancestors(vertex,ancestors)
        return frozenset(ancestors)

    def connect(self,vertex,vertices):
        """Iterates over digraphs produced by making C{vertex} a parent
        to subsets of C{vertices}

        @param vertex: Parent vertex
        @type vertex: Immutable
        @param vertices: Potential children
        @type vertices: Sequence
        @return: Generator of digraphs
        @rtype: Generator iterator
        """
        if vertices:
            first = vertices[0]
            for adg in self.connect(vertex,vertices[1:]):
                yield adg
                adg.add_arrow(vertex,first)
                yield adg
                adg.remove_arrow(vertex,first)
        else:
            yield self
    

    def descendants(self,vertex):
        """Returns the set of all descendants of C{vertex}.
        @param vertex: A vertex in the graph
        @type vertex: Immutable
        @return: descendants of C{vertex}
        @rtype: frozenset of vertices
        """
        descendants = set()
        self._descendants(vertex,descendants)
        return frozenset(descendants)

    def dfsvisit(self,vertex,visited,order):
        """Visit unvisited vertices in depth-first fashion and append
        them to a topological ordering of vertices

        @param vertex: First vertex to visit
        @type vertex: Immutable type (typically String)
        @param visited: Nodes already visited
        @type visited: Set
        @param order: Topological ordering of vertices
        @type order: List
        """
        for parent in self._pa[vertex]:
            if parent not in visited:
                self.dfsvisit(parent,visited,order)
        visited.add(vertex)
        order.append(vertex)


    def enumerate_cycles(self,lim):
        """Enumerates all cycles in a digraph
        of length at most C{lim}

        Ref::


        @article{tarjan:211,
        author = {Robert Tarjan},
        title = {Enumeration of the Elementary Circuits of a Directed Graph},
        publisher = {SIAM},
        year = {1973},
        journal = {SIAM Journal on Computing},
        volume = {2},
        number = {3},
        pages = {211-216},
        keywords = {algorithm; backtracking; circuit; cycle; digraph; graph},
        url = {http://link.aip.org/link/?SMJ/2/211/1},
        doi = {10.1137/0202017}
        }

        @TechReport{tarjan72:_enumer,
        author = 	 {Robert Tarjan},
        title = 	 {Enumeration of the elementary circuits of a directed graph},
        institution =  {Cornell University},
        year = 	 1972,
        number =	 {72-145},
        address =	 {Ithaca, NY},
        month =	 {September}
        }

        Time bound is O((V+E)(C+1)) where C is the (potentially large)
        number of cycles.

        @return: Yields all cycles in the digraph
        @rtype: iterator
        """
        for v in sorted(self._pa):
            for cycle in self._tarjan_backtrack(v,v,set(),[],[],lim-1):
                yield cycle

    def essential_graph(self):
        """Return the essential graph of C{self}

        Uses Chickering's algorithm
        """
        # ORDER-EDGES
        order = self.topological_order()
        order_map = dict(zip(order,range(len(order))))
        ordered_edges = []
        for y_node in order:
            y_parents_plus = [(order_map[x_node],x_node) for x_node in self.parents(y_node)]
            y_parents_plus.sort(reverse=True)
            for x_node_plus in y_parents_plus:
                ordered_edges.append((x_node_plus[1],y_node))

        # LABEL-EDGES
        essential_graph = Graph()
        essential_graph.add_vertices(self.vertices())
        def known(x_node,y_node):
            return x_node in essential_graph.parents(y_node) or \
                   x_node in essential_graph.neighbours(y_node)               
        for (x_node,y_node) in ordered_edges:
            if known(x_node,y_node):
                continue
            y_parents = self.parents(y_node)
            for w_node in essential_graph.parents(x_node):
                if w_node not in y_parents:
                    for y_parent in y_parents:
                        essential_graph.add_arrow(y_parent,y_node)
                    break
                else:
                    essential_graph.add_arrow(w_node,y_node)
            else:
                for z_node in y_parents:
                    if z_node != x_node and z_node not in self.parents(x_node):
                        method = essential_graph.add_arrow
                        break
                else:
                    method = essential_graph.add_line
                for y_parent in y_parents:
                    if not known(y_parent,y_node):
                        method(y_parent,y_node)
        return essential_graph


    @classmethod

    def families(self):
        for v, pa in self._pa.items():
            yield v, frozenset(pa)


    def generate_from_vertices(cls,vertices):
        adg = cls(vertices)
        for gen_adg in adg.generate(vertices):
            yield gen_adg

    def generate(self,vertices):
        """Generate digraphs which respect the order of C{vertices}

        Generates all digraphs produced by adding arrows to C{self} which
        are between vertices in C{vertices} and which respect the order of vertices
        in C{vertices}.

        @param vertices: Ordered vertices
        @type vertices: Tuple
        @return: Generator of  digraphs which respect the order of C{vertices}
        @rtype: Generator iterator
        """
        if vertices:
            rest = vertices[1:]
            for adg in self.generate(rest):
                for adg2 in adg.connect(vertices[0],rest):
                    yield adg2
        else:
            yield self

    def minimal_cycles(self,source):
        """Return all minimal cycles involving  C{source}.

        @param source: Source vertex
        @type source: Immutable
        @return: Minimal cycles
        @rtype: List of tuples of vertices
        """
        cycles = []
        for child in self._ch[source]:
            self._minimaldfs(child,source,(),cycles)
        return cycles


    def minimal_paths(self,source,sink):
        """Return all minimal (non-direct) paths from C{source} to C{sink}.

        A path C{source,v1,v2,.. sink} is minimal if there are no non-consecutive
        vertices joined by an edge in C{self}, (with the possible exception of source to sink)
        and all vertices are distinct.
        (There are no short-cuts provided by edges).
        @param source: Source vertex
        @type source: Immutable
        @param sink: Sink vertex
        @type sink: Immutable
        @return: Minimal paths
        @rtype: List of lists of vertices
        """
        paths = []
        self._minimaldfs(source,sink,(),paths)
        return paths

    def topological_order(self):
        """Return a topological ordering of the vertices

        Children come after their parents in the ordering
        @return: A topological ordering of the vertices
        @rtype: List
        """
        visited = set()
        order = []
        for vertex in self.vertexlist():
            if vertex not in visited:
                self.dfsvisit(vertex,visited,order)
        return order

    def _ancestors(self,vertex,ancestors):
        """Add ancestors of C{vertex} not in C{ancestors} to
        C{ancestors}
        """
        for pa in self._pa[vertex] - ancestors:
            ancestors.add(pa)
            self._ancestors(pa,ancestors)

    def _descendants(self,vertex,descendants):
        """Add descendants of C{vertex} not already in C{descendants} to
        C{descendants}
        """
        for pa in self._pa[vertex] - descendants:
            descendants.add(pa)
            self._descendants(pa,descendants)

    def _minimaldfs(self,current,sink,before_current,paths):
        """current is last vertex on current path
        before_current is all previous vertices
        """
        for child in self._ch[current]:
            # short cuts from source to sink do not count
            for i, v in enumerate(before_current):
                if v == child or (child in self._ch[v] and (i>0 or child != sink)):
                    break #this child no good
            else:
                if child == sink:
                    paths.append(before_current + (current,sink))
                else:
                    self._minimaldfs(child,sink,before_current+(current,),paths)

            

            

    def _tarjan_backtrack(self,start,v,marked,marked_stack,point_stack,lim):
        if len(point_stack) <= lim:
            f = False
            point_stack.append(v)
            marked.add(v)
            marked_stack.append(v)
            for w in self._ch[v]:

                if w < start:
                    continue
                elif w == start:
                    yield tuple(point_stack+[start])
                    f = True
                elif w not in marked:
                    g = False
                    for cycle in self._tarjan_backtrack(start,w,marked,marked_stack,point_stack,lim):
                        yield cycle
                        g = True
                    f = f or g
                if f:
                    while marked_stack[-1] != v:
                        u = marked_stack.pop()
                        marked.remove(u)
            marked_stack.pop() # remove v
            marked.remove(v)
            point_stack.pop() # remove v

class DiForest(DiGraph):
    """Undirected graphs where each connectivity component is a tree
    """
    pass

class ADG(DiGraph):
    """Acyclic directed graphs

    @ivar _an: Maps each vertex to its ancestors
    @type _an: Dictionary
    @ivar _de: Maps each vertex to its descendants
    @type _de: Dictionary
    """
    

    def __init__(self,vertices=(),arrows=(),vertex_positions=None):
        self._an = {}
        self._de = {}
        DiGraph.__init__(self,vertices,arrows,vertex_positions)

    def ancestor(self,an,de):
        """Return whether C{an} is an ancestor of C{de}

        @param an: Potential ancestor
        @type an: Immutable
        @param de: Potential descendant
        @type de: Immutable
        @return: Whether C{an} is an ancestor of C{de}
        @rtype: Boolean
        """
        return an in self._an[de]

    def ancestors(self, vertex):
        return frozenset(self._an[vertex])

    def ancestral_adg(self, vertices):
        """Return the subgraph of the vertices in the small ancestral set
        of C{vertices}"""
        # find the smallest ancestral set of the vertices
        an = set(vertices)
        for vertex in vertices:
            an |= self._an[vertex]

        # find all arrows among vertices in the ancestral set
        arr = []
        for vertex in an:
            for ch in self._ch[vertex] & an:
                arr.append((vertex,ch))
            for pa in self._pa[vertex] & an:
                arr.append((pa,vertex))

        return ADG(vertices=an, arrows=arr)


    def descendants(self, vertex):
        return frozenset(self._de[vertex])

    def add_vertex(self,vertex):
        DiGraph.add_vertex(self,vertex)
        self._an[vertex] = set()
        self._de[vertex] = set()

    def add_arrow(self,frm,to):
        """Add an arrow (directed edge) between two
        existing vertices
 
        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        @raise DirectedCycleError: If the arrow would create a directed cycle
        """
        if to in self._an[frm]:
            raise DirectedCycleError
        DiGraph.add_arrow(self,frm,to)

        #update the ancestors of to and to's descendants
        self._an[to].add(frm)
        self._an[to].update(self._an[frm])
        for de in self._de[to]:
            self._an[de].add(frm)
            self._an[de].update(self._an[frm])

        #update the descendants of frm and frm's ancestors
        self._de[frm].add(to)
        self._de[frm].update(self._de[to])
        for an in self._an[frm]:
            self._de[an].add(to)
            self._de[an].update(self._de[to])

    def connect(self,vertex,vertices):
        """Iterates over all the the ADGs produced by connecting C{vertex} to 
        C{vertices}

        For each vertex in C{vertices}, C{vertex} can be 1)unconnected, 2)a parent
        or 3) a child.
        Assumes C{vertex} initially not connected to any C{vertices} in C{self}
        """
        if vertices:
            first = vertices[0]
            for adg in self.connect(vertex,vertices[1:]):

                # unconnected
                yield adg

                # parent
                try:
                    adg.add_arrow(vertex,first)
                    yield adg
                    adg.remove_arrow(vertex,first)
                except DirectedCycleError:
                    pass

                # child
                try:
                    adg.add_arrow(first,vertex)
                    yield adg
                    adg.remove_arrow(first,vertex)
                except DirectedCycleError:
                    pass
        else:
            yield self

    def connect(self,vertex,vertices):
        """Iterates over all the the ADGs produced by connecting C{vertex} to 
        C{vertices}
        
        For each vertex in C{vertices}, C{vertex} can be 1)unconnected, 2)a parent
        or 3) a child.
        Assumes C{vertex} initially not connected to any C{vertices} in C{self}
        """
        if vertices:
            first = vertices[0]
            for adg in self.connect(vertex,vertices[1:]):

                # unconnected
                yield adg

                # parent
                try:
                    adg.add_arrow(vertex,first)
                    yield adg
                    adg.remove_arrow(vertex,first)
                except DirectedCycleError:
                    pass

                # child
                try:
                    adg.add_arrow(first,vertex)
                    yield adg
                    adg.remove_arrow(first,vertex)
                except DirectedCycleError:
                    pass
        else:
            yield self


    def connect2(self,vertex,vertices):
        """Iterates over all the the ADGs produced by connecting C{vertex} to 
        C{vertices}

        Makes copies to avoid expensive arrow removal

        For each vertex in C{vertices}, C{vertex} can be 1)unconnected, 2)a parent
        or 3) a child.
        Assumes C{vertex} initially not connected to any C{vertices} in C{self}
        """
        if vertices:
            first = vertices[0]
            for adg in self.connect2(vertex,vertices[1:]):

                # unconnected
                yield adg

                # parent
                try:
                    cp = adg.copy()
                    cp.add_arrow(vertex,first)
                    yield cp
                except DirectedCycleError:
                    pass

                # child
                try:
                    cp = adg.copy()
                    cp.add_arrow(first,vertex)
                    yield cp
                except DirectedCycleError:
                    pass
        else:
            yield self.copy()


    def encoding_family(self,inv_atom_ids,predicate='has_parents'):
        """Return the numbers encoding C{self}

        @param inv_atom_ids: Mapping from logical atoms to numbers
        @type inv_atom_ids: Dictionary
        @param predicate: The predicate symbol used for the relation beween a child
        and its parent set
        @type predicate: String
        @return: The numbers encoding C{self}, not in any particular order
        @rtype: List (of positive integers)
        @raise KeyError: if a family is missing
        """
        return [inv_atom_ids[(predicate,child,frozenset(parents))] for child,parents in self._pa.items()]


    def remove_arrow(self,frm,to):
        """
        @todo: Needs improving
        """
        DiGraph.remove_arrow(self,frm,to)

        # expensive
        for an in self._an[frm]:
            for de in self._de[to]:
                if not self._ancestor(an,de):
                    self._an[de].remove(an)
                    self._de[an].remove(de)
            if not self._ancestor(an,to):
                self._an[to].remove(an)
                self._de[an].remove(to)
        for de in self._de[to]:
            if not self._ancestor(frm,de):
                self._an[de].remove(frm)
                self._de[frm].remove(de)
        if not self._ancestor(frm,to):
            self._an[to].remove(frm)
            self._de[frm].remove(to)

    def _ancestor(self,an,de):
        """True if C{an} is an ancestor of C{de}

        Does check without using the _an or _de dictionaries
        So can be used for updating these dictionaries
        """
        for pa in self._pa[de]:
            if pa == an or self._ancestor(an,pa):
                return True
        return False

            

class UGraph(_AbsGraph,_AbsUGraph):
    """Graphs with only undirected edges (i.e. lines)
    """

    def __init__(self,vertices=(),lines=(),vertex_positions=None):
        _AbsGraph.__init__(self,vertices,(),lines,vertex_positions)
        

    def __repr__(self):
        return '%s(%s,%s)' % (
            self.__class__.__name__,
            sorted(self.vertexlist()),
            sorted(self.lines()))

    def extend_compsub(self,compsub,candidates,notset,hypergraph):
        """Extend a complete subset of the graph to find cliques

        This is for Version 1 of Bron and Kerbosch's algorithm. Version 2 is better,
        but more complex.
        
        Checks whether C{candidates} includes all neighbours of some element of C{notset}
        in which case no clique can be formed::

         @Article{bron73:_algor,
         author = 	 {Bron, C. and Kerbosch, J.},
         title = 	 {Algorithm 457: finding all cliques of an undirected graph},
         journal = 	 {Communications of the {ACM}},
         year = 	 1973,
         volume =	 16,
         pages =	 {575--577}
         }
         
        @param compsub: The complete set of vertices to be extended
        @type compsub: Set
        @param candidates: Candidates available to extend C{compsub},
        @type candidates: Set
        @param notset: Vertices that have previously served as candidates and which
        are now explicitly excluded
        @type notset: Set
        @param hypergraph: The graphical hypergraph to which any found cliques will be added.
        @type hypergraph: L{ReducedGraphicalHypergraph}
        """
        # 1-3: This is the branch and bound step. 
        #      Note that notitem will never be removed at line 8
        #      so notset will never become empty
        # 7: For a new clique we need at least one member which does
        #    not neighbour previously processed candidates
        #    if candidate is one of these, then new_notset (line 8) will be
        #    empty

        for notitem in notset:                   # 1
            if candidates <= self._ne[notitem]:  # 2
                return                           # 3
        while candidates:                        # 4 
            candidate = candidates.pop()         # 5 
            compsub.add(candidate)               # 6
            new_candidates = candidates & self._ne[candidate] # 7
            new_notset = notset & self._ne[candidate]         # 8
            self.extend_compsub(compsub,new_candidates,new_notset,hypergraph)
            compsub.remove(candidate)
            notset.add(candidate)
        if not notset:
            hypergraph.add_hyperedge(compsub)

    def twdp(self,up=None,monitor=False):
        """Exact treewidth computation from Bodlaender et al
        """
        v = frozenset(self.vertices())
        n = len(v)
        if up is None:
            up = n - 1
        before = [((),-1)]
        found = False
        for i in range(1,n+1):
            now = {}
            for s, r in before:
                if found:
                    if r >= up:
                        continue
                elif r > up:
                    continue
                ss = set(s)
                for x in v - ss:

                    todo = set([x])
                    done = set([x])
                    while todo:
                        new = set()
                        for vertex in todo:
                            new |= self._ne[vertex]
                        new -= done
                        todo = new & ss
                        done |= new
                    q = len(done - ss) - 1

                    r1 = max(r,q)
                    if found:
                        if r1 >= up:
                            continue
                    elif r1 > up:
                            continue
                        
                    key = frozenset(ss|set([x]))
                    try:
                        t = now[key][2]
                        if r1 < t:
                            now[key] = s,x,r1
                    except KeyError:
                        now[key] = s,x,r1
                    if found:
                        if n-i-1 < up:
                            found = (s+((x),), r1)
                            up = max(r1,n-i-1)
                    elif n-i-1 <= up:
                        found = (s+((x),), r1)
                        up = max(r1,n-i-1)
            before = [(s+((x),), r1) for (s,x,r1) in now.values()]
            if monitor:
                print i, len(before)
        return found

    def gui_triangulate(self,parent,elimination_order,
                        original_colour='black',
                        eliminating_colour='red',
                        uneliminated_neighbour_colour='green',
                        eliminated_colour='grey',
                        fill_in_colour='blue',
                        width=400,
                        height=350,
                        **config):
        """Triangulate the graph using C{elimination_order}
        and display the process graphically.
        
        Alters C{self}

        @param parent: A widget in which the graph will be displayed
        @type parent: A suitable Tkinter object, eg a Frame.
        @param original_colour: Colour for uneliminated vertices not
        participating in an elimination
        @type original_colour: String
        @param eliminating_colour: Colour for the vertex being eliminated
        @type eliminating_colour: String
        @param uneliminated_neighbour_colour: Colour for uneliminated neighbours
        of the the vertex being eliminated
        @type uneliminated_neighbour_colour: String
        @param eliminated_colour: Colour for eliminated vertices
        @type eliminated_colour: String
        @param fill_in_colour: Colour for fill-in lines
        @type fill_in_colour: String
        @param elimination_order: The elimination order
        @type elimination_order: An iterator (typically a list)
        @param config: Configuration options for the GUI
        @type config: Various
        @return: The triangulated graph
        @rtype: Same class as C{self}
        """
        gc = GraphCanvas(self,parent,edit=False,
                         colour_user_actions=False,
                         width=width,height=height,**config)
        gc.pack()
        bottom = Tkinter.Frame(parent)
        bottom.pack()
        labels = []
        for vertex in elimination_order:
            label = Tkinter.Label(bottom,text=vertex)
            labels.append(label)
            label.pack(side=Tkinter.LEFT)
        eliminated = set()
        i = [0]
        def handler(self=self, elimination_order=elimination_order,
                    eliminated=eliminated,gc=gc,
                    original_colour=original_colour,
                    eliminating_colour=eliminating_colour,
                    uneliminated_neighbour_colour=uneliminated_neighbour_colour,
                    eliminated_colour=eliminated_colour,
                    fill_in_colour=fill_in_colour,i=i,labels=labels
                    ):
            j=i[0]
            if j > 0:
                labels[j-1].config(fg=eliminated_colour)
            try:
                labels[j].config(fg=eliminating_colour)
                i[0]=j+1
            except IndexError:
                labels[j-1].config(fg=eliminated_colour)
            return self._gui_elim_one(elimination_order,eliminated,gc,
                                      original_colour,eliminating_colour,
                                      uneliminated_neighbour_colour,
                                      eliminated_colour,fill_in_colour)
        Tkinter.Button(bottom,text='Next',command=handler).pack()


    def gui_maximum_cardinality_search(
        self,parent,choose=None,width=400,height=350,**config):

        if choose is None:
            def choose(set): return set.pop()

        vertices = self.vertices()    # 1
        sets = []                     # 2
        alpha = {}                    # 3
        alpha_inv = []                # 4
        cardinality = {}              # 5 
        for vertex in vertices:       # 6
            cardinality[vertex] = 0   # 7
            sets.append(set())        # 8
            alpha_inv.append(None)    # 9
        sets[0] = vertices            #10
        j = [0]                       #12
        i = [len(vertices)-1]

        gc = GraphCanvas(self,parent,edit=False,
                         colour_user_actions=False,
                         width=width,height=height,**config)
        gc.pack()
        bottom = Tkinter.Frame(parent)
        bottom.pack()
        label = Tkinter.Label(bottom,text=str(alpha_inv))
        label.pack(side=Tkinter.LEFT)
        def handler(self=self,i=i,j=j,alpha=alpha,alpha_inv=alpha_inv,
                    cardinality=cardinality,sets=sets,choose=choose):
            if not sets[j[0]]:
                return
            v=self._gui_maxcard_one(i,j,alpha,alpha_inv,cardinality,sets,choose)
            gc.vertex_config(v,fill='grey')
            label.config(text=str(alpha_inv))
        Tkinter.Button(bottom,text='Next',command=handler).pack()

    def hypergraph(self):
        """Return the clique hypergraph of an undirected graph

        Uses Bron and Kerbosch algorithm, see L{extend_compsub}
        @return: The clique hypergraph of an undirected graph
        @rtype: L{ReducedGraphicalHypergraph}
        """
        cliques = ReducedGraphicalHypergraph()
        self.extend_compsub(set([]),self.vertices(),set([]),cliques)
        return cliques

        clique_hypergraph = hypergraph

    def is_triangulated(self):
        """Return whether the graph is triangulated

        Uses L{maximum_cardinality_search}, see that method for acknowledgements

        @return: Whether the graph is triangulated (aka 'decomposable', 'chordal')
        @rtype: Boolean
        """
        return self.triangulate(
            self.maximum_cardinality_search()[0],zero_fillin_check=True)

    is_chordal = is_triangulated

    is_decomposable = is_triangulated

    def maximum_cardinality_search(self,choose=None):
        """Return maximum cardinality search

        Ref::

         @Article{tarjan84:_simpl,
         author = 	 {Robert E. Tarjan and Mihalis Yannakakis},
         title = 	 {Simple linear-time algorithms to test chordality of graphs, test acyclicity of hypergraphs, and selectively reduce acyclic hypergraphs}, 
         journal = 	 {{SIAM} Journal of Computing},
         year = 	 1984,
         volume =	 13,
         number =	 3,
         pages =	 {566--579},
         month =	 {August}
        """

        # As Tarjan and Yannakakis put it the basic idea is:
        # "Number the vertices from n to 1 in decreasing order. As the next
        # vertex to number, select the vertex adjacent to the largest number
        # of previously numbered vertices, breaking ties arbitrarily".

        # The following implements their algorithm, execpt that numbering is from
        # n-1 to 0, and the user can specify a tie-breaker (the 'choose' function)
        # if desired.

        # Lines 1-12 are initialisation. The key data structure is the dictionary
        # 'cardinality' which throughout maps each unnumbered vertex to the number of
        # numbered vertices adjacent to it. The list 'sets' is such that sets[i]
        # is a set containing all unnumbered vertices with cardinality i. Initially no vertices
        # are numbered so all vertices have cardinality zero (lines 7 and 10).
        # alpha and alpha_inv will contain the vertex numbering on return. alpha maps
        # each vertex to its number, and alpha_inv is just the inverse mapping (see lines
        # 17 and 18). Eventually all the None values in alpha_inv (line 9) will be replaced
        # by the appropriate integers. j is the largest value such that sets[j] is non-empty.

        # Line 9a adds a dummy empty set so that j in line 24 never causes an IndexError

        # In the main loop (lines 13-26) we number vertices from i=n-1 down to i=0
        # (The 'range' function (13) generates a list with precisely this sequence of integers.)
        # The loop invariant is that 'cardinality', 'sets' and 'j' maintain the
        # properties described above. In particular,
        # sets[j] contains all unnumbered vertices with maximum cardinality.

        # Line 14 assigns v to be a maximum cardinality vertex and removes it
        # from sets[j]. This assignment is then stored (lines 15,16)

        # Now that v has been numbered the cardinalities of all its unnumbered
        # neighbours [given by the set self._ne[v].difference(alpha) in line 17 ]
        # need to be incremented.
        # Lines 17-22 update 'sets' and 'cardinality' accordingly. Lines 23-35
        # maintain the loop invariant for j
        
        if choose is None:
            def choose(set): return set.pop()
        
        vertices = self.vertices()    # 1
        sets = []                     # 2
        alpha = {}                    # 3
        alpha_inv = []                # 4
        cardinality = {}              # 5 
        for vertex in vertices:       # 6
            cardinality[vertex] = 0   # 7
            sets.append(set())        # 8
            alpha_inv.append(None)    # 9
        sets.append(set())            # 9a
        sets[0] = vertices            #10
        j = 0                         #12
        
        for i in range(len(vertices)-1,-1,-1): # 13
            v = choose(sets[j])   # 14
            alpha[v] = i          # 15
            alpha_inv[i] = v      # 16
            for w in self._ne[v].difference(alpha): # 17
                card_w = cardinality[w]             # 18
                sets[card_w].remove(w)              # 19
                card_w += 1                         # 20
                sets[card_w].add(w)                 # 21
                cardinality[w] = card_w             # 22
            j += 1                  # 23
            while j >= 0 and sets[j] == emptyset:  # 24
                j -= 1                             # 25
        return alpha, alpha_inv     # 26

    def put_line(self,frm,to):
        """Add a line (undirected edge) to the graph between two
        existing vertices
 
        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        """
        self._ne[frm].add(to)
        self._ne[to].add(frm)

    def random_graph(self,n,m):
        n0 = len(self._ne)
        if sorted(self._ne) != range(n0):
            raise ValueError("Method only works on graphs with vertices named 0,1,..n")
        self.add_vertices(range(n0,n0+n))
        for new_edge in random.sample(xrange(n*n),m):
            self.put_line(n0 + (new_edge/n), n0 + (new_edge%n))
        return self
                
        
    #     def junction_tree(self,elimination_order):
    #         """Return a junction tree"""
    #         old_cliques = {}
    #         eliminated = set()
    #         waiting = set()
    #         forest = UForest()
    #         for v in elimination_order:
    #             message = self._ne[v] - eliminated
    #             parents = old_cliques.get(v,set()) & waiting
    #             for waiting_clique in parents:
    #                 if message <= waiting_clique:
    #                     v_clique = waiting_clique
    #                     parents.remove(v_clique)
    #                     break
    #             else:
    #                 v_clique = frozenset(message | set([v]))
    #                 waiting.add(v_clique)
    #                 forest.add_vertex(v_clique)
    #                 self.complete(message)
    #             for waiting_clique in parents:
    #                 forest.put_line(waiting_clique,v_clique)
    #                 waiting.remove(waiting_clique)
    #             for var in message:
    #                 try:
    #                     old_cliques[var].add(v_clique)
    #                 except KeyError:
    #                     old_cliques[var] = set(v_clique)
    #             eliminated.add(v)
    #         return forest


   
    
    def zero_fillin_check(self,elimination_order=None):
        if elimination_order is None:
            elimination_order = self.maximum_cardinality_search()[0]
        eliminated = set()
        for vertex in elimination_order:
            message = self._ne[vertex] - eliminated
            while message:
                nbr = message.pop()
                for other_nbr in message:
                    if other_nbr not in self._ne[nbr]:
                        return False
            eliminated.add(vertex)
        return self

    def triangulate2(self,elimination_order):
        """Triangulate the graph B{inefficiently!} using  C{elimination_order}

        Triangulates the graph by eliminating vertices in the order given by
        C{elimination_order}. When a vertex is eliminated all its uneliminated
        neighbours must be a complete set (all pairwise connected by edges). Extra edges
        are added, if necessary, to ensure this is the case.

        'Eliminated' vertices are not actually deleted, just marked as 'eliminated',
        so you don't end up with an empty graph!

        @param elimination_order: The elimination order
        @type elimination_order: An iterator (typically a list)
        """
        eliminated = set()
        for vertex in elimination_order:
            message = self._ne[vertex] - eliminated
            self.complete(message)
            eliminated.add(vertex)
        return self

    def triangulate(self,elimination_order,zero_fillin_check=False,modify=True):
        """Triangulate the graph using C{elimination_order}

        Alters C{self} by default

        Ref::

         @Article{tarjan84:_simpl,
         author = 	 {Robert E. Tarjan and Mihalis Yannakakis},
         title = 	 {Simple linear-time algorithms to test chordality of graphs, test acyclicity of hypergraphs, and selectively reduce acyclic hypergraphs}, 
         journal = 	 {{SIAM} Journal of Computing},
         year = 	 1984,
         volume =	 13,
         number =	 3,
         pages =	 {566--579},
         month =	 {August}
        
        @param elimination_order: The elimination order
        @type elimination_order: An iterator (typically a list)
        @param zero_fillin_check: If set the method returns whether C{elimination_order} is a zero
        fill-in and does not alter C{self}
        @type zero_fillin_check: Boolean
        @param modify: Whether to add the fill-in edges to C{self}
        @type modify: Boolean
        @return: (As long as C{zero_fillin_check=False}), the fill-in (added edges) produced by C{elimination_order}
        @rtype: (As long as C{zero_fillin_check=False}), the list of tuples, each tuple is an unordered pair of vertices
        """
        # This algorithm exploits the notion of a vertex's
        # *follower*. The follower of vertex v is the earliest vertex
        # in the elimination order coming after v which is adjacent to
        # v in the elimination graph (i.e. the graph formed by adding
        # the fill-in edges). A vertex need not have a follower - the
        # last vertex never has one.

        # Define f*(x) to be the {x, f(x), f(f(x)), ... }, ie the path
        # of followers starting with x. Theorem 3 of the above
        # reference establishes the following. If v comes before w in
        # the elimination ordering then there is a v-w edge in the
        # elimination graph if and only if there is a vertex x which
        # is (i) adjacent to w in the orginal graph and (ii) where v
        # is a member of f*(x)

        # This result leads to the following algorithm. The vertices
        # are processed in order.  For each vertex w the neighbours
        # which come before w are considered.  For each of these
        # neighbours x, the vertices in f*(x) coming before w are
        # found and connected to w.

        # A variant of this is implemented as follows. Dictionary f
        # stores the `follower' mapping and the list fill_in records
        # fill-in edges. They are initialised in lines 1 and
        # 2. Vertices w are considered according to the given order
        # (line 3).

        # A dummy value of w for the follower of w is assigned (line
        # 4). self._ne[w] on line 5 is the set of neighbours of w. By
        # intersecting this with the dictionary f the variable
        # `earlier_neighbours' contains only neighbours of w earlier
        # in the elimination order. The set `done' (of which more
        # later) is these neighbours together with w.

        # Each earlier neighbour x is considered in turn (line 7). If
        # the follower of x has yet to be found then f[x] in line 8 is
        # x otherwise it is the follower of x. In the latter case, the
        # chain of followers of x are considered (lines 9-13) until we
        # hit one that has already been `done'. Any vertex in such a
        # chain is a follower of an earlier neighbour of w which is
        # not itself an existing neighbour of w and so a fill-in edge
        # is needed (line 11). x at line 11 cannot be an existing
        # neighbour since all earlier neighbours are in the set `done'
        # and x is not (line 9) and all later neighbours cannot be
        # reached yet by a chain of followers. Note that the set
        # `done' also prevents revisiting sequences of followers due
        # to line 10. Lines 9a and 9b provide an early exit if all that
        # is required is a check for a zero fill-in.

        # If the x at the end of one of these chains is found to
        # contain a dummy value for its follower this is replaced by
        # the true value - w - in lines 13-14.

        # Lines 16-18 add the fill-in edges to the graph (if that is
        # required) and return the fillin-edges. Lines 14a and 14b return
        # the appropriate boolean value if a check is being done.
        
        f = {}          # 1
        fill_in = []    # 2
        for w in elimination_order:                          # 3
            f[w] = w                                         # 4
            earlier_neighbours = self._ne[w].intersection(f) # 5
            done = set([w]) | earlier_neighbours             # 6
            for x in earlier_neighbours:           #  7
                x = f[x]                           #  8
                while x not in done:               #  9
                    if zero_fillin_check:            #  9a
                        return False                 #  9b
                    done.add(x)                    # 10
                    fill_in.append((x,w))          # 11
                    x = f[x]                       # 12
                if f[x] == x:                      # 13
                    f[x] = w                       # 14
        if zero_fillin_check:                         # 14a
            return True                               # 14b
        if modify:                   # 15
            self.put_lines(fill_in)  # 16
        return fill_in               # 17

#     def triangulate(self,elimination_order):
#         """Tarjan and Yannakakis version
#         """
#         f = {}
#         index = {}
#         new_edges = []
#         for i, w in enumerate(elimination_order):
#             f[w] = w
#             index[w] = i
#             for v in self._ne[w].intersection(f):
#                 x = v
#                 while index[x] < i:
#                     index[x] = i
#                     if x not in self._ne[w]:
#                         new_edges.append((x,w))
#                     x = f[x]
#                 if f[x] == x:
#                     f[x] = w
#         self.put_lines(new_edges)
#         return new_edges



    def _gui_elim_one(self,elimination_order,eliminated,gc,
                      original_colour,eliminating_colour,
                      uneliminated_neighbour_colour,
                      eliminated_colour,fill_in_colour):
        if not elimination_order:
            for v in self.vertexlist():
                gc.vertex_config(v,fill=eliminated_colour)
            return
        vertex = elimination_order.pop(0)
        message = self._ne[vertex] - eliminated
        for v in self.vertexlist():
            if v == vertex:
                gc.vertex_config(v,fill=eliminating_colour)
            elif v in message:
                gc.vertex_config(v,fill=uneliminated_neighbour_colour)
            elif v in elimination_order:
                gc.vertex_config(v,fill=original_colour)
            else:
                gc.vertex_config(v,fill=eliminated_colour)
        msg = message.copy()
        while message:
            v1 = message.pop()
            for v2 in message - self._ne[v1]:
                gc.drawarc_vertices(v1,v2)
                gc.arc_config(v1,v2,fill=fill_in_colour)
        self.complete(msg)
        eliminated.add(vertex)

    def _gui_maxcard_one(self,i,j,alpha,alpha_inv,cardinality,sets,choose):
        i0=i[0]
        j0=j[0]
        v = choose(sets[j0])   # 14
        alpha[v] = i0          # 15
        alpha_inv[i0] = v      # 16
        for w in self._ne[v].difference(alpha): # 17
            card_w = cardinality[w]             # 18
            sets[card_w].remove(w)              # 19
            card_w += 1                         # 20
            sets[card_w].add(w)                 # 21
            cardinality[w] = card_w             # 22
        j0 += 1                           # 23
        while j0 >= 0 and not sets[j0]:    # 24
            j0 -= 1                       # 25
        i[0] -= 1
        j[0]=j0
        return v


class UForest(UGraph):
    """Undirected graphs where each connectivity component is a tree

    @ivar _co: Maps each vertex to the set of all other vertices in its connectivity component
    @type _co: Dictionary
    """

    def __init__(self,vertices=(),lines=(),vertex_positions=None):
        self._co = {}
        UGraph.__init__(self,vertices,lines,vertex_positions)

    def add_vertex(self,vertex):
        """Adds a vertex to a graph

        @param vertex: The vertex to add
        @type vertex: Anything immutable
        @raise ExistingVertexError: If vertex already exists
        """
        UGraph.add_vertex(self,vertex)
        self._co[vertex] = set([vertex])

    def put_line(self,frm,to):
        """Add a line (undirected edge) to the graph between two
        existing vertices
 
        @param frm: One of the vertices connected by the line
        @type frm: Immutable type
        @param to: The other vertex connected by the line
        @type to: Immutable type
        @raise KeyError: If either vertex does not exist
        @raise CycleError: If adding the line would create a cycle
        """
        if frm in self._co[to]:
            raise CycleError
        UGraph.put_line(self,frm,to)
        self._co[frm].update(self._co[to])
        self._co[to].update(self._co[frm])



            
        
        

class EssentialGraph(Graph):

    import copy
    

    def __init__(self, *args, **kwargs):
        super(EssentialGraph,self).__init__(*args, **kwargs)
        self._constrained_order = None

    def adjacents(self,vertex):
        return self._ne.get(vertex,emptyset) | self._ch.get(vertex,emptyset) | self._pa.get(vertex, emptyset)

    def copy(self):
        return copy.deepcopy(self)

    def constrain_order(self, order):
        self._constrained_order = order

    def directed_path(self,a,b):
        # bfs.
        if a == b:
            return True

        fringe = [a]
        while fringe:
            next = set()
            for a in fringe:
                ch = self.children(a)
                if b in ch:
                    return True
                next |= ch
            fringe = next
        return False

    @staticmethod
    def from_graph(graph):
        graph.__class__ = EssentialGraph
        graph._constrained_order = None
        return graph

    def is_potential_parent(self, parent, child):
        # NOTE: must NOT be applied to R1-R4 below since these constraints by
        # themselves have been proven necessary (verma and pearl) and
        # sufficient (meeks, 1995)
        if self._constrained_order is None:
            return True

        # it had better be the case that child and parent are in constrained
        # order.
        if child not in self._constrained_order or parent not in self._constrained_order:
            return True

        # does child come after parent in the constrained order?
        return self._constrained_order.index(child) > self._constrained_order.index(parent)

    def no_arrow_loops(self):
        for a, b in self.arrows():
            if self.directed_path(b, a):
                raise DirectedCycleError,'loop detected between '+str((a,b))

    def orient(self, keep_class=False):
        """Orient an essential graph into a DAG. The class of self becomes ADG unless C{keep_class}
        From @inproceedings{meek95,
                author = {C. Meek},
                title = {Causal inference and causal explanation with background knowledge},
                year = {1995},
                booktitle = {Proceedings of the Eleventh Conference on Uncertainty in Artificial Intelligence},
                editor = {S. Hanks and P. Besnard},
                pages = {403--410},
                publisher = {Morgan Kaufmann}
            }
        """
        lines = self.lines()
        while lines:
            # pick an undirected edge
            edge = lines.pop()

            # skip if removed
            if edge[0] not in self.neighbours(edge[1]):
                continue

            # orient it according to ordering constraint (if any)
            self.remove_line(edge[0],edge[1])
            if self.is_potential_parent(edge[0], edge[1]):
                self.add_arrow(edge[0], edge[1])
            else:
                self.add_arrow(edge[1], edge[0])

            # propagate the implications of this orientation
            self.resolve()

        if not keep_class:
            self = ADG(vertices=self.vertices(), arrows=self.arrows())

        return self

    def resolve(self):
        # add as many directed edges as possible such that no new v-structures
        # nor cycles are created.
        self.no_arrow_loops()
        progress = True
        while progress:
            progress = False
            # these were proved to be sufficient (meek, 1995) and are from pearl (2000)
            progress |= self._complete_chain()
            self.no_arrow_loops()
            progress |= self._bounce_potential_cycles()
            self.no_arrow_loops()
            progress |= self._complete_colliders()
            self.no_arrow_loops()
            progress |= self._bounce_chain()
            self.no_arrow_loops()

    # impose order constraint

    def _orient_from_order(self):
        if self._constrained_order is None:
            return
        lines = self.lines()
        for a,b in lines:
            if not self.is_potential_parent(a, b):
                self.remove_line(a,b)
                self.add_arrow(b, a)
            elif not self.is_potential_parent(b, a):
                self.remove_line(a,b)
                self.add_arrow(a, b)


    # graph operators for resolution

    def _complete_chain(self):
        # complete a -> b - c => a -> b -> c
        # since we have added all colliders.
        # R1
        progress = False
        for a,b in self.arrows():
            new_arrows = set()
            for c in self.neighbours(b) - self.adjacents(a):
                new_arrows.add((b,c))

            for b,c in new_arrows:
                progress = True
                self.remove_line(b,c)
                self.add_arrow(b,c)
        return progress

    def _bounce_potential_cycles(self):
        # if there exists a directed edge a -> c -> b, and an undirected edge a -
        # b, then it must be the case that a -> b; else a cycle would be
        # created.
        # R2
        progress = False
        for a,b in self.lines():
            if self._bounce_cycles(a,b):
                progress = True
            else:
                progress |= self._bounce_cycles(b,a)
        return progress

    def _bounce_cycles(self,a,b):
        # if a directed path exists A -> C -> B and A - B, then it must be that A
        # -> B otherwise a cycle would be created (A - B must exist).
        if not (self.children(a) & self.parents(b)):
            return False
        self.remove_line(a,b)
        self.add_arrow(a,b)
        return True

    def _complete_colliders(self):
        # given:
        #        a
        #     /  |  \
        #    c   |   d
        #    _\| | |/_
        #        b
        # orient as:
        #        a
        #     /  |  \
        #    c   |   d
        #    _\|\|/|/_
        #        b
        # given an undirected edge A - B, and two non-adjacent nodes (C,D) making B
        # a collider, direct A -> B. XXX: justification?
        # R3
        progress = False
        for c,b in self.arrows():
            done = False
            n_c = frozenset([c]) | self.adjacents(c)
            p_b_nc = self.parents(b) - n_c
            for a in self.neighbours(b) & self.neighbours(c):
                for d in p_b_nc & self.neighbours(a):
                    progress = True
                    self.remove_line(a,b)
                    self.add_arrow(a,b)
                    done = True
                    break
                if done:
                    break
        return progress

    def _bounce_chain(self):
        # given:
        #        a
        #     /  |  \
        #    b _ *   c
        #    |\  | |/_
        #        d
        # orient as:
        #        a
        #    |/_ |  \
        #    b _ *   c
        #    |\  | |/_
        #        d
        # given an undirected edge A - B, with a chain C -> D -> B, such that
        # B and C are non-adjacent and A and D are adjacent, orient A -> B.
        # R4
        progress = False
        for d,b in self.arrows():
            done = False
            n_b = frozenset([b]) | self.adjacents(b)
            p_d_nb = self.parents(d) - n_b
            for a in self.neighbours(b) & self.adjacents(d):
                for c in p_d_nb & self.neighbours(a):
                    progress = True
                    self.remove_line(a,b)
                    self.add_arrow(a,b)
                    done = True
                    break
                if done:
                    break
        return progress

class MutilatedADG(ADG):
    @staticmethod
    def from_adg(adg):
        madg = adg.copy()
        madg.__class__ = MutilatedADG
        madg._mutilated_edges = {}
        return madg

    def __init__(self):
        super(MutilatedADG, self).__init__()
        self._mutilated_edges = {}

    # copied from _AbsGraph:
    # $Id: Graphs.py,v 1.2 2008/10/07 09:09:58 jc Exp $
    def copy(self):
        """Return a copy a graph

        @return: A copy of the graph
        @rtype: Same class as C{self}
        """
        cp = self.__class__()
        for attr, val in vars(self).items():
            if attr == '_vertex_positions':
                setattr(cp,attr,val.copy())
                continue
            setattr(cp,attr,{})
            for vertex, vertices in val.items():
                if hasattr(vertices,'copy'):
                    getattr(cp,attr)[vertex] = vertices.copy()
                else:
                    getattr(cp,attr)[vertex] = vertices
        return cp

    def remove_arrow(self, src, dst):
        super(MutilatedADG, self).remove_arrow(src, dst)
        self._mutilated_edges[src] = dst

    def mutilated_edges(self):
        return self._mutilated_edges.iteritems()
