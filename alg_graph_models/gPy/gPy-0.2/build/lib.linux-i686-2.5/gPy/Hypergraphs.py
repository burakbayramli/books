"""Hypergraphs

@var _version: Version of this module
@type _version: String
"""

import Tkinter
from Utils import member, emptyset, pretty_str_set
from IO import GraphCanvas

_version = '$Id: Hypergraphs.py,v 1.2 2008/10/07 09:10:14 jc Exp $'

class HypergraphError(Exception):
    pass

class RedundancyError(HypergraphError):
    pass

class GraphicalityError(HypergraphError):
    pass

class DecomposabilityError(HypergraphError):
    pass


class Incidence(object):
    """
    Graphs and hypergraphs are implemented differently, so even though hypergraphs are a
    generalisation of graphs there is no direct inheritance relation defined for the
    relevant classes.

    The name 'Incidence' should not be taken too seriously. There's nothing here specific to incidence
    structures.

    This abstract class contains those few methods which are sufficiently abstract to work
    for both graphs and hypergraphs
    """

    def reachable(self,a,s):
        """Return all vertices not in C{a} to which there is a path from a vertex in C{a}
        which includes no vertices in C{s}

        'Vertices' in C{s} which are not in the hypergraph/graph are silently ignored.

        @param a: Set of vertices
        @type a: Iterable
        @param s: Blocking set of vertices
        @type s: Iterable
        @return: Vertices not in C{a} reachable from C{a} by a path avoiding C{s}
        @rtype: List
        @raise ValueError: If C{a} and C{s} are not disjoint
        @raise KeyError: If C{a} contains a non-existent vertex
        """
        banned = set(s)
        if banned.intersection(a):
            raise ValueError("Sets are not disjoint")
        banned.update(a)
        frontier = list(a)
        reachable = []
        while frontier:
            new_vertices = self.neighbours(frontier.pop()) - banned
            reachable.extend(new_vertices)
            frontier.extend(new_vertices)
            banned.update(new_vertices)
        return reachable


    def separates(self,a,b,s):
        """Return whether C{s} separates C{a} from C{b} in C{self}

        'Vertices' in C{s} which are not in the graph/hypergraph are silently ignored.

        @param a: Set of vertices
        @type a: Iterable
        @param b: Set of vertices
        @type b: Iterable
        @param s: Set of vertices possibly separating C{a} from C{b}
        @type s: Iterable
        @return: Whether C{s} separates C{a} from C{b}
        @rtype: Boolean
        @raise ValueError: If C{a}, C{b} and C{s} are not disjoint
        @raise KeyError: If C{a} or C{b} contain a non-existent vertex
        """
        a, b, s = set(a), set(b), frozenset(s)
        if len(a|b|s) != len(a) + len(b) + len(s):
            raise ValueError("Sets are not disjoint")
        a_open, b_open = list(a), list(b)
        a_seen, b_seen = a|s, b|s
        while a_open:
            new_vertices = self.neighbours(a_open.pop(0)) - a_seen
            if new_vertices & b:
                return False
            a_seen.update(new_vertices)
            a.update(new_vertices)
            a_open.extend(new_vertices)
            a, b = b, a
            a_open, b_open = b_open, a_open
            a_seen, b_seen = b_seen, a_seen
        return True


class Hypergraph(Incidence):
    """A hypergraph is a collection of hyperedges. A hyperedge is a set of vertices.

    The base set is defined implicitly as the union of the hyperedges.
    @ivar _hyperedges: The distinct hyperedges in the hypergraph. Each hyperedge is a frozenset
    @type _hyperedges: Set
    @ivar _star: Maps a vertex to the set of hyperedges which
    contain it. (Effectively represents the I{dual} hypergraph.)
    @type _star: Dictionary
    @ivar _extras: If repeated hyperedges exist, maps each repeated hyperedge to the
    number of times it is repeated. (If repeated once, the hyperedge occurs twice.)
    @type _extras: Dictionary
    """

    def __init__(self,hyperedges=()):
        """Initialise a hypergraph

        @param hyperedges: Initial hyperedges (or an existing hypergraph)
        @type hyperedges: Iterable of iterables (or L{Hypergraph})
        @raise TypeError: If C{hyperedges} is not of the right form.
        """
        if isinstance(hyperedges,Hypergraph):
            self.__dict__ = hyperedges.__dict__
        else:
            self._hyperedges = set()
            self._star = {}
            for hyperedge in hyperedges:
                self.add_hyperedge(hyperedge)

    def __contains__(self,hyperedge):
        """Return whether a hyperedge is in the hypergraph

        @param hyperedge: hyperedge
        @type hyperedge: Iterable
        @return: Whether C{hyperedge} is in the hypergraph
        @rtype: Boolean
        """
        return frozenset(hyperedge) in self._hyperedges

    def __eq__(self,other):
        """Return whether C{self} and C{other} are
        identical hypergraphs (not merely isomorphic)

        Irrespective of class
        @param other: Hypergraph
        @type other: L{Hypergraph}
        @return: C{self} and C{other} are identical hypergraphs
        @rtype: Boolean
        """
        if self._hyperedges == other._hyperedges:
            if hasattr(self,'_extras'):
                for hyperedge, reps in self._extras.items():
                    try:
                        if other._extras[hyperedge] != reps:
                            return False
                    except (AttributeError, KeyError):
                        return False
            elif hasattr(other,'_extras'):
                return False
            return True
        else:
            return False

    def __ge__(self,other):
        """Return whether C{self} >= C{other}

        C{self} >= C{other} if every hyperedge in C{other} is contained in a
        hyperedge in C{self}
       
        Irrespective of class
        @param other: Hypergraph
        @type other: L{Hypergraph}
        @return: Whether C{self} >= C{other}
        @rtype: Boolean
        """
        return other <= self

    def __getitem__(self,vertex):
        """Return the star of C{vertex}: the set of hyperedges which contain C{vertex}

        B{Altering the returned set will alter the hypergraph, so don't do it!}
        A safer option is to use the L{star} method.
        Note that a set object is returned, not a hypergraph object.

        @param vertex: The vertex whose star is sought
        @type vertex: String
        @return: The star of C{vertex}: the hyperedges each of which contain C{vertex}
        @rtype: Set
        @raise KeyError: If C{vertex} is not in the (hyper)graph
        """
        return self._star[vertex]


    def __iter__(self):
        """Return an iterator over the B{distinct} hyperedges in the hypergraph
        
        To allow C{for hyperedge in hypergraph: ...} constructions.
        @return: An iterator over the B{distinct} hyperedges in the hypergraph
        @rtype: Iterator
        """
        return iter(self._hyperedges)

    def __le__(self,other):
        """Return whether C{self} <= C{other}
        
        C{self} <= C{other} if every hyperedge in C{self} is contained in a
        hyperedge in C{other}
        
        Irrespective of class
        @param other: Hypergraph
        @type other: L{Hypergraph}
        @return: Whether C{self} <= C{other}
        @rtype: Boolean
        """
        for hyperedge in self:
            if not other.would_be_redundant(hyperedge):
                return False
        return True

    def __len__(self):
        """Return the number of hyperedges in the hypergraph

        @return: The number of hyperedges in the hypergraph
        @rtype: Int
        """
        n = len(self._hyperedges)
        try:
            for m in self._extras.values():
                n += m
        except AttributeError:
            pass
        return n


    def __ne__(self,other):
        """Return whether C{self} and C{other} are
        not identical hypergraphs

        Irrespective of class
        @param other: Hypergraph
        @type other: L{Hypergraph}
        @return: Whether C{self} and C{other} are not identical hypergraphs
        @rtype: Boolean
        """
        return not (self == other)

    def __repr__(self):
        """Return 'official' string representation of the hypergraph

        Can be used to construct the hypergraph using e.g. C{eval}
        @return: The 'official' string representation of the hypergraph
        @rtype: String
        """
        if self.__class__ == Hypergraph:
            return 'Hypergraph(%s)' % self._hyperedges
        else:
            return '%s(Hypergraph(%s))' % (self.__class__.__name__, self._hyperedges)

    def __str__(self):
        """Return pretty representation of a hypergraph

        @return: Pretty representation of a hypergraph
        @rtype: String
        """
        hs = []
        for hyperedge in self._hyperedges:
            hs.append(', '.join([pretty_str_set(hyperedge)] * (self.multiplicity(hyperedge))))
        return '( %s )' % ', '.join(hs)
    
    def add_hyperedge(self,hyperedge):
        """Add a hyperedge 

        @param hyperedge: The vertices in the hyperedge
        @type hyperedge: Iterable
        """
        hyperedge = frozenset(hyperedge)
        if hyperedge in self._hyperedges:
            try:
                self._extras[hyperedge] += 1
            except AttributeError:
                self._extras = {hyperedge:1}
            except KeyError:
                self._extras[hyperedge] = 1
        else:
            self._add_hyperedge(hyperedge)

    def copy(self):
        """Return a deep copy of the hypergraph

        @return: A copy of C{self}
        @rtype: The same as C{self}
        """
        hg = Hypergraph()
        hg._hyperedges.update(self._hyperedges)
        for vertex, hyperedges in self._star.items():
            hg._star[vertex] = hyperedges.copy()
        if self.__class__ == Hypergraph:
            if hasattr(self,'_extras'):
                hg._extras = self._extras.copy()
            return hg
        else:
            return self.__class__(hg,check=False)


    def discard_hyperedge(self,hyperedge):
        """Remove a hyperedge (and any repetitions),
        or do nothing if the hyperedge does not exist

        @param hyperedge: The hyperedge to be removed
        @type hyperedge: Iterable
        """
        try:
            self.remove_hyperedge(hyperedge)
        except KeyError:
            pass

    def discard_hyperedges(self,hyperedges):
        """For each hyperedge in C{hyperedges}, remove it (and any repetitions),
        or do nothing if it does not exist

        @param hyperedges: Hyperedges to be removed
        @type hyperedges: Iterable
        """
        for hyperedge in hyperedges:
            self.discard_hyperedge(hyperedge)

    def discard_vertex(self,vertex):
        """Remove C{vertex} from the hypergraph, or do nothing if it is not there

        @param vertex: The vertex to remove
        @type vertex: String
        @raise RedundancyError: If C{self} is of class L{ReducedHypergraph} (or one of
        its subclasses) and a redundant hyperedge is produced
        """
        try:
            self.remove_vertex(vertex)
        except KeyError:
            pass

    def discard_vertices(self,vertices):
        """Remove C{vertices} from the hypergraph
        
        @param vertices: The vertices to remove
        @type vertices: Iterable
        @raise RedundancyError: If C{self} is of class L{ReducedHypergraph} (or one of
        its subclasses) and a redundant hyperedge is produced
        """
        for vertex in vertices:
                self.discard_vertex(vertex)

    def dual(self):
         """Return the dual of the hypergraph

         @todo: make more efficient
         """
         mp = {}
         # map each hyperedge to a set of numbers,
         # these will be the new vertices
         for i, h in enumerate(self.hyperedges()):
             try:
                 mp[h].append(i)
             except KeyError:
                 mp[h] = [i]
         # each vertex produces an edge in the dual
         new_edges = []
         for hs in self._star.values():
             new_edge = []
             for h in hs:
                 new_edge.extend(mp[h])
             new_edges.append(new_edge)
         return Hypergraph(new_edges)

    def grahams(self):
        """Run Graham's algorithm on a hypergraph

        If the hypergraph is decomposable then upon return C{self} is either
        empty or contains a single empty hyperedge.
        """
        if self.is_empty():
            return self
        
        self.red()

        cardinality = {}
        sets = [set()]
        max = 0
        for vertex, star in self._star.items():
            card = len(star) - 1
            cardinality[vertex] = card
            if card > max:
                for i in range(card-max):
                    sets.append(set())
                max = card
            sets[card].add(vertex)
            
        while sets[0] != set():
            new_hs = self.remove_vertices(sets[0])
            sets[0] = set()
            for hyperedge in new_hs:
                if self.is_redundant(hyperedge):
                    self.remove_hyperedge_once(hyperedge)
                    for vertex in hyperedge:
                        card = cardinality[vertex]
                        sets[card].remove(vertex)
                        card -= 1
                        cardinality[vertex] = card
                        sets[card].add(vertex)
        return self
    
    def gui_grahams_algorithm(self,parent,width=400,height=300,**config):

        import Graphs

        def add_to_set(dkt,key,member):
            try:
                dkt[key].add(member)
            except KeyError:
                dkt[key] = set([member])

        state, labels = {}, {}
        top = Tkinter.Frame(parent)
        top.pack()
        state_frame = Tkinter.Frame(top)
        state_frame.pack(side=Tkinter.LEFT)
        Tkinter.Label(state_frame,text='Original').grid()
        Tkinter.Label(state_frame,text='Current').grid(column=1,row=0)
        for i, hyperedge in enumerate(self._hyperedges):
            state[hyperedge] = set(hyperedge)
            Tkinter.Label(state_frame,text=pretty_str_set(hyperedge)).grid(row=i+1,sticky=Tkinter.W)
            labels[hyperedge] = Tkinter.Label(state_frame,text=pretty_str_set(hyperedge))
            labels[hyperedge].grid(column=1,row=i+1,sticky=Tkinter.W)

        cardinality = {}
        in_only_one = {}
        for vertex, hyperedges in self._star.items():
            nh = len(hyperedges)
            if nh == 1:
                add_to_set(in_only_one,member(hyperedges),vertex)
            else:
                cardinality[vertex] = nh
        join_forest = Graphs.UForest(self._hyperedges)  

        graph = self.two_section()
        gc = GraphCanvas(graph,top,edit=False,
                         colour_user_actions=False,
                         width=width,height=height,**config)
        gc.pack(side=Tkinter.LEFT)
        bottom = Tkinter.Frame(parent)
        bottom.pack()
        gcj = GraphCanvas(join_forest,bottom,edit=False,
                          colour_user_actions=False,
                          pp_vertex=pretty_str_set,
                         width=width,height=height,**config)
        gcj.pack()
        varslabel = Tkinter.Label(bottom)
        varslabel.pack(side=Tkinter.LEFT)
        sel = frozenset()
        for h, vs in in_only_one.items():
            if sorted(h) > sorted(sel):
                v = vs
                sel = h
        if len(v) == 1:
            print 'About to remove vertex', tuple(v)[0], 'since it is only present in', pretty_str_set(sel) 
        else:
            print 'About to remove vertices', tuple(v), 'since they are only present in', pretty_str_set(sel) 
        def handler(self=self,in_only_one=in_only_one,state=state,
                    cardinality=cardinality,labels=labels,
                    gc=gc,gcj=gcj,varslabel=varslabel,join_forest=join_forest,
                    add_to_set=add_to_set):
            self._gui_graham(in_only_one,state,cardinality,labels,
                             gc,gcj,varslabel,join_forest,add_to_set)
        Tkinter.Button(bottom,text='Next',command=handler).pack()

    def _gui_graham(self,in_only_one,state,cardinality,labels,
                    gc,gcj,varslabel,join_forest,add_to_set):
        if not in_only_one:
            print 'Finished'
            return
        hyperedge = frozenset()
        for h, vs in in_only_one.items():
            if sorted(h) > sorted(hyperedge):
                hyperedge = h
                vertices = vs
        del in_only_one[hyperedge]
        #hyperedge, vertices = in_only_one.popitem()
        residue = state[hyperedge]                   
        residue -= vertices
        labels[hyperedge].config(text=pretty_str_set(residue))
        for vertex in vertices:
            gc.vertex_config(vertex,fill='grey')
        if residue:              
            cp = residue.copy()                                                         
            v = cp.pop()                                                                
            proper_supersets = self._star[v] - set([hyperedge]) 
            proper_supersets.intersection(state)                                        
            for vertex in cp:                                                           
                proper_supersets &= self._star[vertex]          
                if not proper_supersets:                                                
                    break                                                               
            if proper_supersets:                          
                del state[hyperedge]
                labels[hyperedge].config(fg='grey')
                receiver = member(proper_supersets)       
                join_forest.put_line(hyperedge,receiver)
                gcj.drawarc_vertices(hyperedge,receiver)
                for vertex in residue:                           
                    nh = cardinality[vertex]                     
                    if nh == 2:                                  
                        add_to_set(in_only_one,receiver,vertex)  
                    else:                                        
                        cardinality[vertex] = nh - 1             
        else:                     
            del state[hyperedge]
        sel = frozenset()
        for h, vs in in_only_one.items():
            if sorted(h) > sorted(sel):
                v = vs
                sel = h
        if sel:
            if len(v) == 1:
                print 'About to remove vertex', tuple(v)[0], 'since it is only present in', pretty_str_set(sel) 
            else:
                print 'About to remove vertices', tuple(v), 'since they are only present in', pretty_str_set(sel) 
        elif len(state) > 1:
            flag = False
            print 'No vertex is in only one hyperedge'
            for hyperedge, current in state.items():
                for h, c in state.items():
                    if current is not c and current <= c:
                        flag = True
                        print 'Removing', pretty_str_set(current), 'since it is contained in', pretty_str_set(c)
                        del state[hyperedge]
                        labels[hyperedge].config(fg='grey')
                        join_forest.put_line(hyperedge,h)
                        gcj.drawarc_vertices(hyperedge,h)
                        for vertex in hyperedge:                           
                            nh = cardinality[vertex]                     
                            if nh == 2:                                  
                                add_to_set(in_only_one,receiver,vertex)  
                            else:                                        
                                cardinality[vertex] = nh - 1
            if not flag:
                print 'No redundant hyperedges'
            sel = frozenset()
            for h, vs in in_only_one.items():
                if sorted(h) > sorted(sel):
                    v = vs
                    sel = h
            if sel:
                if len(v) == 1:
                    print 'About to remove vertex', tuple(v)[0], 'since it is only present in', pretty_str_set(sel) 
                else:
                    print 'About to remove vertices', tuple(v), 'since they are only present in', pretty_str_set(sel) 
    
    def gui_display(self,parent,**config):
        """Display a hypergraph using a GUI

        Shows the representative graph

        Ignores repeated hyperedges, so not quite accurate for non-simple hypergraphs

        @param parent: Parent window for GUI
        @type parent: Suitable Tkinter widget
        @param config: Configuration options for the GUI
        @type config: Various
        @return: The GUI
        @rtype: L{GraphCanvas}
        """
        return self.representative_graph().gui_display(parent,**config)

    def hyperedges(self):
        """Return a list of the hyperedges in the hypergraph

        @return: A list of the hyperedges in the hypergraph
        @rtype: List
        """
        hs = []
        for h in self._hyperedges:
            hs.extend([h] * self.multiplicity(h))
        return hs

    def is_arboreal(self):
        """Return whether the hypergraph is arboreal

        A hypergraph is arboreal iff its dual is acyclic (ie decomposable)

        @return: Whether the hypergraph is decomposable
        @rtype: Boolean
        """
        return self.dual().is_decomposable()

    def is_decomposable(self):
        """Return whether the hypergraph is decomposable

        Uses L{maximum_cardinality_search}

        @return: Whether the hypergraph is decomposable
        @rtype: Boolean
        """
        return bool(self.maximum_cardinality_search(decomp_check=True))

    is_acyclic = is_decomposable

    def is_empty(self):
        return self._hyperedges == set()
         

    def is_graphical(self):
        """Return whether the hypergraph is graphical

        @return: Whether the hypergraph is graphical
        @rtype: Boolean
        @todo: Implement efficiently
        """
        if self._hyperedges == set():
            return True
        r = ReducedHypergraph(self.copy(),modify=True)
        return r == r.two_section().hypergraph()

    is_conformal = is_graphical

    def is_reduced(self):
        """Test whether a hypergraph is reduced

        @return: Whether a hypergraph is reduced
        @rtype: Boolean
        """
        if self._hyperedges == set():
            return True
        if hasattr(self,'_extras'):
            return False
        if emptyset in self._hyperedges:
            if len(self._hyperedges) > 1:
                return False
            else:
                return True
        done = set()
        for star in self._star.values():
            for hyperedge in star.difference(done):
                for other_hyperedge in star:
                    if other_hyperedge is not hyperedge and other_hyperedge >= hyperedge:
                        return False
                done.add(hyperedge)
        return True


    def is_redundant(self,hyperedge,check=False):
        """Whether C{hyperedge} is a redundant hyperedge of the hypergraph

        @param hyperedge: Hyperedge
        @type hyperedge: Iterable
        @return: False if not redundant, otherwise the set of distinct
        containing hyperedges
        @rtype: Boolean or set
        @raise ValueError: If C{check=True} and C{hyperedge} is not in the hypergraph.
        So this is only really useful for testing the redundancy of existing hyperedges.
        To test the redundancy of hyperedges which may not already be in the hypergraph
        use L{makes_unreduced}.
        """
        if hyperedge not in self:
            if check:
                raise ValueError("%s not in %s" % (hyperedge,self))
            else:
                return False
        if hyperedge:
            for vertex in hyperedge:
                try:
                    cs.intersection_update(
                        self._star[vertex])
                except NameError:
                    cs = self._star[vertex].copy()
                if len(cs) == 1:
                    if not hasattr(self,'_extras') or hyperedge not in self._extras:
                        return False
        else:
            cs = self._hyperedges.copy()
        if not hasattr(self,'_extras') or hyperedge not in self._extras:
            cs.remove(hyperedge)
        return cs

    def is_separating(self):
        """Whether the hypergraph is a separating hypergraph

        A hypergraph is separating if, for every vertex v, the intersection
        of all hyperedges in v's star equals {v}

        @return: Whether the hypergraph is a separating hypergraph
        @rtype: Boolean
        """
        for hyperedges in self._star.values():
            tmp = hyperedges.copy()
            st = tmp.pop()
            while len(st) > 1:
                try:
                    st &= tmp.pop()
                except KeyError:
                    return False
        return True


    def is_simple(self):
        """Test whether a hypergraph is simple

        @return: Whether a hypergraph is simple
        @rtype: Boolean
        """
        return not hasattr(self,'_extras')

    def join(self,hypergraph):
        """Return the join of C{self} and C{hypergraph}

        @return: C{self ^ hypergraph}
        @rtype: ReducedHypergraph
        """
        join_hg = SimpleHypergraph()
        for he1 in self._hyperedges:
            for he2 in hypergraph._hyperedges:
                try:
                    join_hg.add_hyperedge(he1 & he2)
                # if he1 & he2 already there
                except ValueError:
                    pass
        join_hg.make_reduced()
        return join_hg


    def join_forest(self,choose=None):
        """Return a join forest graph, assuming decomposability

        Any repeated hyperedges are ignored

        Uses L{maximum_cardinality_search}. See that method for bibliographical
        references.
        @return: A join forest graph
        @rtype: L{Graphs.UForest}
        @raise DecomposabilityError: If hypergraph is not decomposable
        """
        import Graphs
        result = self.maximum_cardinality_search(choose,True)
        if result:
            receiver = result[1] 
            join_forest = Graphs.UForest(self._hyperedges.copy())
            for sender, recipient in receiver.items():
                join_forest.put_line(sender,recipient)
        else:
            raise DecomposabilityError("%s is not decomposable" % self)
        return join_forest




    def join_forest_ve(self):
        """Return a join forest for the hypergraph using vertex elimination

        Source is annotated.
        Refs::
        
          @TechReport{graham79,
           author = 	 {M. H. Graham},
           title = 	 {On the universal relation},
           institution =  {University of Toronto},
           year = 	 1979,
           address =	 {Toronto, Canada}
         }

         @article{322389,
         author = {Catriel Beeri and Ronald Fagin and David Maier and Mihalis Yannakakis},
         title = {On the Desirability of Acyclic Database Schemes},
          journal = {Journal of the  {ACM}},
          volume = {30},
          number = {3},
          year = {1983},
          issn = {0004-5411},
          pages = {479--513},
          doi = {http://doi.acm.org/10.1145/2402.322389},
          publisher = {ACM Press},
          address = {New York, NY, USA},
         }

        @return: A join forest for the hypergraph
        @rtype: L{Graphs.UForest}
        @raise RedundancyError: If hypergraph is not reduced
        @raise ValueError: If the hypergraph is reduced but not decomposable
        """

        # Graham's algorithm is as follows:
        # Apply the following two operations until neither applies:
        # 1. Delete a vertex that it is in only one hyperedge
        # 2. Delete a redundant hyperedge
        # If the hypergraph is decomposable all vertices will eventually be deleted.

        # Beeri et al show that Graham's algorithm can be used to construct
        # a join tree from a decomposable hypergraph. The vertices of the join tree are the
        # hyperedges of the hypergraph. If a non-empty hyperedge is deleted at step 2 of Graham's algorithm
        # then, in the join tree, an edge is drawn between it and one of the hyperedges containing it.

        # This method implements this approach. The only difference from Graham's algorithm
        # is that in step 1 all 'isolated' vertices in the selected hyperedge are removed. Only
        # once this is done is there any prospect of the hyperedge being redundant.

        # Since hyperedges will be altered we set up 'state' (lines 1-3) which maps hyperedges
        # from their (immutable) initial state to their
        # (mutable) current state. If a hyperedge is deleted it is removed from 'state'.
        # In lines 4-11 is set up the mapping 'in_only_one'. A hyperedge is a key in this mapping if it
        # contains a vertex which appears in no other edges (an 'isolated' vertex).
        # The value associated with this key is
        # just the set of vertices only appearing in the hyperedge. If a vertex appears in more than one
        # hyperedge, we record how many in the mapping 'cardinality'.

        # The join forest is initialised to have the hyperedges as vertices, and no edges at line 12.

        # The main loop is executed as long as there is a vertex contained in only one edge (line 13).

        # A hyperedge containing vertices not found elsewhere is selected and these vertices are
        # then deleted from it (lines 14-16). If residual vertices exist in the hyperedge (line 17), we
        # need to check for redundancy. Lines 18-25 do this by constructing 'proper_supersets': the set of
        # remaining (line 21) hyperedges containing all vertices in 'residue'.
        # If (line 26) this is non-empty, then the hyperedge is removed (line 27) and an arbitrary
        # member of proper_supersets is chosen for the required connection in the join tree (line 28).
        # If a hyperedge has been removed it is necessary to update 'in_only_one'
        # and 'cardinality' appropriately (lines 30-35).

        # Lines 36,37: If there were no residual vertices then the hyperedge has become empty.
        # So, if there are
        # any other remaining hyperedges, it will be redundant, but no join forest edges need be drawn.

        # Termination: If there remain undeleted edges (and hence undeleted vertices) then the hypergraph
        # was not decomposable (lines 38,39)
        # otherwise all is well and the join forest can be returned (line 40).

        import Graphs
        
        if not self.is_reduced():
            raise RedundancyError("%s is not reduced" % self)
        
        def add_to_set(dkt,key,member):
            try:
                dkt[key].add(member)
            except KeyError:
                dkt[key] = set([member])

        state = {}                               # 1
        for hyperedge in self._hyperedges:       # 2
            state[hyperedge] = set(hyperedge)    # 3
        cardinality = {}                                                       # 4
        in_only_one = {}                                                       # 5
        for vertex, hyperedges in self._star.items():  # 6
            nh = len(hyperedges)                                               # 7
            if nh == 1:                                                        # 8
                add_to_set(in_only_one,member(hyperedges),vertex)              # 9
            else:                                                              #10
                cardinality[vertex] = nh                                       #11
        join_forest = Graphs.UForest(self._hyperedges)  # 12
        while in_only_one:    # 13
            hyperedge, vertices = in_only_one.popitem()  # 14
            residue = state[hyperedge]                   # 15
            residue -= vertices                          # 16
            if residue:              # 17
                cp = residue.copy()                                                         # 18
                v = cp.pop()                                                                # 19
                proper_supersets = self._star[v] - set([hyperedge]) # 20
                proper_supersets.intersection(state)                                        # 21
                for vertex in cp:                                                           # 22
                    proper_supersets &= self._star[vertex]          # 23
                    if not proper_supersets:                                                # 24
                        break                                                               # 25
                if proper_supersets:                          # 26
                    del state[hyperedge]                      # 27
                    receiver = member(proper_supersets)       # 28
                    join_forest.put_line(hyperedge,receiver)  # 29
                    for vertex in residue:                           # 30
                        nh = cardinality[vertex]                     # 31
                        if nh == 2:                                  # 32
                            add_to_set(in_only_one,receiver,vertex)  # 33
                        else:                                        # 34
                            cardinality[vertex] = nh - 1             # 35
            else:                     # 36
                del state[hyperedge]  # 37
        if state:                                                                           # 38
            raise ValueError("Hypergraph was not decomposable, no join forest is possible") # 39
        return join_forest # 40

    def make_decomposable(self,criterion=None):
        """Return a L{DecomposableHypergraph} produced by eliminating variables from C{self}
        using C{criterion}

        Destroys C{self} in the process.
        
        @param criterion: Function giving criterion for choosing next vertex to eliminate
        @type criterion: Function
        @return: Decomposable hypergraph, destination dictionary
        @rtype: Tuple
        """
        if criterion is None:
            def criterion(hg):
                first = True
                for v, star in hg._star.items():
                    card = len(star)
                    if card == 1:
                        return v
                    if first or card < minimum:
                        minimum = card 
                        optimal = v
                        first = False
                return optimal
        dhg = DecomposableHypergraph()
        arrows = {}
        original_hyperedges = self._hyperedges.copy()
        destination = {}
        while self._star:
            vertex = criterion(self)
            new_hyperedge = set()
            originals = []
            # make the new hyperedge produced by eliminating
            for hyperedge in self._star[vertex].copy():
                new_hyperedge |= hyperedge
                if hyperedge in original_hyperedges:
                    originals.append(hyperedge)
                self.remove_hyperedge(hyperedge)
            if not dhg.would_be_redundant(new_hyperedge):
                frozen_new_hyperedge = frozenset(new_hyperedge)
                dhg.add_hyperedge(frozen_new_hyperedge)
                dest = frozen_new_hyperedge 
            else:
                # hack!
                for hyperedge in dhg._hyperedges:
                    if new_hyperedge <= hyperedge:
                        dest = hyperedge
                        break
            for original in originals:
                destination[original] = dest
            new_hyperedge.remove(vertex)
            if new_hyperedge not in self:
                self.add_hyperedge(new_hyperedge)
        return dhg, destination


    def make_decomposable2(self,elimination_ordering=None):
        """Return a L{DecomposableHypergraph} produced by eliminating variables from C{self}
        in the order given by C{elimination_ordering}.

        Destroys C{self} in the process.
        
        If no C{elimination_ordering} is given
        L{Graphs.UGraph.maximum_cardinality_search} is used
        to provide one.

        @todo: This is simple but inefficient at present.

        @param elimination_ordering: Order in which to eliminate variables
        @type elimination_ordering: Sequence
        @return: Decomposable hypergraph, destination dictionary
        @rtype: Tuple
        """
        if elimination_ordering is None:
            # order_dict = ReducedHypergraph(
            #    self.copy(),modify=True).maximum_cardinality_search()[0]
            # would be better to do this directly on the hypergaph
            order_dict = self.two_section().maximum_cardinality_search()[0]
            elimination_ordering = [None] * len(order_dict)
            for variable, index in order_dict.items():
                elimination_ordering[index] = variable
        dhg = DecomposableHypergraph()
        original_hyperedges = self._hyperedges.copy()
        destination = {}
        for vertex in elimination_ordering:
            new_hyperedge = set()
            originals = []
            for hyperedge in self._star[vertex].copy():
                new_hyperedge |= hyperedge
                if hyperedge in original_hyperedges:
                    originals.append(hyperedge)
                self.remove_hyperedge(hyperedge)
            if not dhg.would_be_redundant(new_hyperedge):
                frozen_new_hyperedge = frozenset(new_hyperedge)
                dhg.add_hyperedge(frozen_new_hyperedge)
                dest = frozen_new_hyperedge
            else:
                # hack!
                for hyperedge in dhg._hyperedges:
                    if new_hyperedge <= hyperedge:
                        dest = hyperedge
                        break
            for original in originals:
                destination[original] = dest
            new_hyperedge.remove(vertex)
            if new_hyperedge not in self:
                self.add_hyperedge(new_hyperedge)
        return dhg, destination



    def makes_unreduced(self,hyperedge):
        """True if C{hyperedge} is a subset or superset of a  hyperedge in the
        hypergraph ...

        ... irrespective of whether C{self} is already reduced or not
        @param hyperedge: Hyperedge
        @type hyperedge: Iterable
        @return: Whether C{hyperedge} (would) cause redundancy
        """
        hyperedge = frozenset(hyperedge)
        #needs to be made more efficient
        for he in self._hyperedges:
            if he >= hyperedge or he <= hyperedge:
                return True
        return False

    def matrix(self,sort=False):
        """Return the incidence matrix for a hyperedge
        """
        hyperedges = self.hyperedges()
        if sort:
            ky = {}
            for h in hyperedges:
                ky[h] = sorted(h)
            def kyf(x): return ky[x]
            hyperedges.sort(key=kyf)
            vertices = sorted(self._star)
        else:
            vertices = self._star
        cols = {}
        for col, h in enumerate(hyperedges):
            try:
                cols[h].append(col)
            except KeyError:
                cols[h] = [col]
        matrix = []
        zeros = [0] * len(hyperedges)
        for vertex in vertices:
            row = zeros[:]
            for hyperedge in self._star[vertex]:
                for col in cols[hyperedge]:
                    row[col] = 1
            matrix.append(row)
        return matrix

    def matrix_generate(self,matrix,transpose=False):
        """Add hyperedges, vertices using an incidence matrix
        """
        rn = range(len(matrix[0]))  # no. of hyperedges
        rm = range(len(matrix))     # no. of vertices
        if transpose:
            for row in rm:
                hyperedge = []
                for col in rn:
                    if matrix[row][col] == 1:
                        hyperedge.append(col)
                self.add_hyperedge(hyperedge)
        else:
            for col in rn:
                hyperedge = []
                for row in rm:
                    if matrix[row][col] == 1:
                        hyperedge.append(row)
                self.add_hyperedge(hyperedge)
        return self

    def msc_decompcover(self):
        """DON'T USE: STILL BEING WRITTEN"""
        def getmax(s):
            l = 0
            for x in s:
                if len(x) > l:
                    l = len(x)
                    biggest = x
            s.remove(biggest)
            return biggest
        eliminated_in, receiver = self.maximum_cardinality_search(getmax)
        newhs = {}
        for hyperedge in self._hyperedges:
            newhs[hyperedge] = set(hyperedge)
            #TODO propogate properly
        for sender, receiver in receiver.items():
            newhs[receiver] |= sender - eliminated_in.get(sender,frozenset())
        return Hypergraph(newhs.values())

        
    def maximum_cardinality_search(self,
                                   choose=None,decomp_check=False):
        """
        Multiple hyperedges are ignored, so if the hypergraph is non-simple this produces the
        same result as if multiple hyperedges had been deleted.

        @param choose: Function for breaking ties when selecting a hyperedge with
        maximum cardinality. If C{None} ties are broken arbitrarily.
        @type choose: A function which removes and returns an element from a set.
        @param decomp_check: If C{True} then C{False} is returned as soon as it is
        established that C{self} is not decomposable
        @type decomp_check: Boolean
        @return: If C{decomp_check=True} and C{self} is not decomposable then
        C{False} is returned. Otherwise (1) For each selected hyperedge the set
        of vertices eliminated there
        and (2) for each hyperedge the hyperedge (if any) which `absorbs' it
        @rtype: C{False} or Tuple, a pair of dictionaries
        """
        if choose is None:
            def choose(s): return s.pop()

        sets = [self._hyperedges.copy()]
        cardinality = dict(
            zip(self._hyperedges,[0] * len(self._hyperedges)))
        eliminated_in = {}
        eliminated_vertices = set()
        receiver = {}

        def ok(h):
            return (h - eliminated_in.get(h,frozenset())
                    <= receiver.get(h,frozenset()))
                    
        while sets:
            if not sets[-1]:
                sets.pop()
                continue
            selected_hyperedge = choose(sets[-1])
            eliminated_here = (
                selected_hyperedge - eliminated_vertices)
            eliminated_vertices.update(eliminated_here)
            eliminated_in[selected_hyperedge] = eliminated_here
            if decomp_check and not ok(selected_hyperedge):
                return False
            else:
                del cardinality[selected_hyperedge]
            for vertex in eliminated_here:
                for hyperedge in (
                    self._star[vertex].intersection(cardinality)):
                    receiver[hyperedge] = selected_hyperedge
                    card_h = cardinality[hyperedge]
                    sets[card_h].remove(hyperedge)
                    card_h += 1
                    if card_h == len(hyperedge):
                        if decomp_check and not ok(hyperedge):
                            return False
                        else:
                            del cardinality[hyperedge]
                    else:
                        cardinality[hyperedge] = card_h
                        try:
                            sets[card_h].add(hyperedge)
                        except IndexError:
                            sets.append(set([hyperedge]))
        return eliminated_in, receiver

        

    def merge(self,hyperedges):
        """Replace hyperedges by their union.

        All repetitions of a hyperedge in C{hyperedges} will go.

        @param hyperedges: The hyperedges to merge
        @type hyperedges: Iterable
        """ 
        new_hyperedge = set()
        for hyperedge in hyperedges:
            hyperedge = frozenset(hyperedge)
            new_hyperedge.update(hyperedge)
            self.remove_hyperedge(hyperedge)
        self.add_hyperedge(frozenset(new_hyperedge))

    def multiplicity(self,hyperedge):
        """Return how often C{hyperedge} occurs in the hypergraph

        Returns 0 if C{hyperedge} is not in the hypergraph

        @param hyperedge: hyperedge
        @type hyperedge: Iterable
        @return: How often C{hyperedge} occurs in the hypergraph
        @rtype: Int
        @raise KeyError: If C{hyperedge} contains vertices not in the hypergraph
        """
        hyperedge = frozenset(hyperedge)
        try:
            return self._extras[hyperedge] + 1
        except (AttributeError, KeyError):
            if hyperedge in self._hyperedges:
                return 1
            elif hyperedge <= self.vertices():
                return 0
            else:
                raise KeyError('%s contains vertices not in %s' % (hyperedge, self.vertices()))

    def neighbours(self,vertex):
        """Return the set of all those vertices each of which is in
        a hyperedge with C{vertex}

        C{vertex} is not included amongst the neighbours

        @param vertex: The vertex whose neighbours are sought
        @type vertex: String
        @return: Neighbours of C{vertex}
        @rtype: Set
        """
        neighbours = set()
        for hyperedge in self._star[vertex]:
            neighbours.update(hyperedge)
        neighbours.remove(vertex)
        return neighbours

    def order(self):
        """Return the number of vertices in the hypergraph

        @return: The number of vertices in the hypergraph
        @rtype: Int
        """
        return len(self._star)
        

    def red(self):
        """Reduce the hypergraph
        
        Does not change the class of C{self}
        
        Any hyperedge contained in another is removed
        @return: Any redundant hyperedges
        @rtype: List
        """
        deleted = self.simplify()
        redundant = self.red_hyperedges()
        for redundant_hyperedge in redundant:
            self.remove_hyperedge(redundant_hyperedge)
        deleted.extend(list(redundant))
        return deleted

    #     def reduced(self):
    #         """Return a new hypergraph which is the reduced version of
    #         C{self}
    
    #         Does not alter C{self}
    #         @return: The reduced version of C{self}
    #         @rtype: L{ReducedHypergraph}
    #         """
    #         cp = self.copy()
    #         cp.make_reduced()
    #         return cp

    def red_hyperedges(self):
        """Returns the set of redundant hyperedges in the hypergraph.

        Multiple occurrences of a hyperedge are ignored.

        @return: Set of redundant hyperedges
        @rtype: Set
        """
        if emptyset in self._hyperedges and len(self._hyperedges) > 1:
            red = set([emptyset])
        else:
            red = set()
        done = set()
        for star in self._star.values():
            for hyperedge in star.difference(done):
                for other_hyperedge in star:
                    if other_hyperedge is not hyperedge and other_hyperedge >= hyperedge:
                        red.add(hyperedge)
                        break
                done.add(hyperedge)
        return red


    def redundant_hyperedges(self,only_redundant=True):
        """Returns a dictionary mapping each distinct strictly redundant hyperedge to
        the set of distinct hyperedges which properly contain it.

        A strictly redundant hyperedge is one properly contained in another

        @return: Set of superset hyperedges for each redundant hyperedge
        @rtype: Dictionary
        """
        if emptyset in self._hyperedges:
            supersets = {emptyset:self._hyperedges - set(emptyset)}
        else:
            supersets = {}
        for star in self._star.values():
            for hyperedge in star.difference(supersets):
                supersets[hyperedge] = set()
                for other_hyperedge in star:
                    if other_hyperedge is not hyperedge and other_hyperedge >= hyperedge:
                        supersets[hyperedge].add(other_hyperedge)
        if only_redundant:
            for hyperedge, supsets in supersets.items():
                if supsets == set():
                    del supersets[hyperedge]
        return supersets

    def remove_hyperedge(self,hyperedge):
        """Remove a hyperedge (including any repetitions of it)

        @param hyperedge: The hyperedge to be removed
        @type hyperedge: Iterable
        @raise KeyError: If C{hyperedge} is not in the hypergraph
        """
        hyperedge = frozenset(hyperedge)
        self._hyperedges.remove(hyperedge)
        for vertex in hyperedge:
            self._star[vertex].remove(hyperedge)
            if not self._star[vertex]:
                del self._star[vertex]
        try:
            del self._extras[hyperedge]
        except (AttributeError, KeyError):
            pass

    def remove_hyperedge_once(self,hyperedge):
        """Remove one occurrence of a hyperedge

        If the hyperedge is repeated a repetition is removed.

        @param hyperedge: The hyperedge to be removed
        @type hyperedge: Iterable
        @raise KeyError: If C{hyperedge} is not in the hypergraph
        """
        hyperedge = frozenset(hyperedge)
        try:
            n = self._extras[hyperedge]
            if n == 1:
                del self._extras[hyperedge]
            else:
                self._extras[hyperedge] = n-1
        except (AttributeError, KeyError):
            self.remove_hyperedge(hyperedge)

    def remove_hyperedges(self,hyperedges):
        """Remove distinct hyperedges

        Each hyperedge in C{hyperedges} is removed together with any repetitions

        @param hyperedges: Distinct hyperedges to be removed
        @type hyperedges: Sequence
        @raise KeyError: If some hyperedge in C{hyperedges} does not exist
        """
        for hyperedge in hyperedges:
            self.remove_hyperedge(hyperedge)



    def remove_vertex(self,vertex):
        """Remove C{vertex} from the hypergraph

        @param vertex: The vertex to remove
        @type vertex: String
        @raise KeyError: If C{vertex} is not in the hypergraph
        @raise RedundancyError: TOFIX: If C{self} is of class L{ReducedHypergraph} (or one of
        its subclasses) and a redundant hyperedge is produced
        """
        return self.remove_vertices(frozenset([vertex]))
#         new_hs = []
#         for hyperedge in self._star[vertex]:
#             smaller = hyperedge - set([vertex])
#             new_hs.append(smaller)
#             self._hyperedges.remove(hyperedge)
#             try:
#                 del self._extras[hyperedge]
#             except (AttributeError, KeyError):
#                 pass
#             if smaller in self._hyperedges:
#                 if not isinstance(self,SimpleHypergraph):
#                     try:
#                         self._extras[smaller] += 1
#                     except AttributeError:
#                         self._extras = {smaller:1}
#                     except KeyError:
#                         self._extras[smaller] = 1
#             else:
#                 self._hyperedges.add(smaller)
#             for v in smaller:
#                 tmp = self._star[v]
#                 tmp.remove(hyperedge)
#                 tmp.add(smaller)
#         del self._star[vertex]
#         return new_hs
                

    def remove_vertices(self,vertices):
        """Remove C{vertices} from the hypergraph

        @param vertices: The vertices to remove
        @type vertices: Set
        @raise KeyError: If C{vertex} is not in the hypergraph
        @raise RedundancyError: If C{self} is of class L{ReducedHypergraph} (or one of
        its subclasses) and a redundant hyperedge is produced
        """
        return self.restriction(vertices,True)

    def rename_vertices(self,renaming,check=True):
        """Rename the vertices of a hypergraph

        New hypergraph is isomorphic to existing one.

        @param renaming: Map from existing vertices to new vertices
        @type renaming: Dictionary
        @return: Isomorphic hypergrah with new names for vertices
        @rtype: Same class as C{self}
        @raise KeyError: If an existing vertex is missing from C{renaming}
        @raise ValueError: If two existing vertices are mapped to the name new name
        """
        if check:
            chk_st = set()
            for newname in renaming.values():
                if newname in chk_st:
                    raise ValueError('%s appears more than once as a new name' % newname)
                chk_st.add(newname)
        newhs = []
        for hyperedge in self._hyperedges:
            newh = []
            for v in hyperedge:
                newh.append(renaming[v])
            newhs.extend([newh] * self.multiplicity(hyperedge))
        return self.__class__(newhs)


    def representative_graph(self):
        """Return the representative graph of a hypergraph

        The vertices of the graph are hyperedges of the hypergraph. Two
        vertices are connected if the corresponding hyperedges intersect

        Ignores repeated hyperedges, so not quite accurate for non-simple hypergraphs

        Also known as the line-graph of the hypergraph
        
        @return: The representative graph of the hypergraph
        @rtype: L{UGraph}
        """
        import Graphs
        rg = Graphs.UGraph(self._hyperedges)
        for star in self._star.values():
            rg.complete(star)
        return rg

    def restriction(self,vertices,inverted=False):
        """Restrict the hypergraph to contain only C{vertices}

        @param vertices: The vertices to restrict the hypergraph to,
        unless C{inverted=True} in which case the vertices to remove.
        @type vertices: Iterable
        @param inverted: Whether to remove C{vertices}
        @type inverted: Boolean
        @return: New hyperedges produced
        @rtype: List
        """
        if inverted:
            going = vertices
            staying = self.vertices().difference(vertices)
        else:
            staying = frozenset(vertices)
            going = self.vertices() - staying
        done = set()
        smallers = []
        for v in going:
            try:
                for h in self._star[v] - done:
                    smaller = h & staying
                    done.add(h)
                    smallers.append(smaller)
                    self.remove_hyperedge(h)
                    self.add_hyperedge(smaller)
            except KeyError:
                pass
        return smallers
    
    def simplify(self):
        """Simplify a hypergraph

        Returning repeated hyperedges deleted

        @return: Repeated hyperedges
        @rtype: list of frozensets
        """
        reps = []
        try:
            for hyperedge, rep in self._extras.items():
                reps.extend([hyperedge]*rep)
            del self._extras
        except AttributeError:
            pass
        return reps

    size = __len__

    def star(self,vertex):
        """Return the star of C{vertex}: the set of hyperedges which contain C{vertex}

        Altering the returned set B{will not} alter the hypergraph.
        Note that a set object is returned, not a hypergraph object.

        @param vertex: The vertex whose star is sought
        @type vertex: String
        @return: The star of C{vertex}: the hyperedges each of which contain C{vertex}
        @rtype: Set
        @raise KeyError: If C{vertex} is not in the (hyper)graph
        """
        return self._star[vertex].copy()


    def star_size(self,vertex):
        """Return a count of the number of distinct hyperedges containing a vertex

        @param vertex: The vertex for which containing hyperedges are sought
        @type vertex: String
        @return: The number of hyperedges each of which contain C{vertex}
        @rtype: Int
        @raise KeyError: If C{vertex} is not in the (hyper)graph
        """
        return len(self._star[vertex])

    def stars(self,vertices):
        """Return the hyperedges each of which contain at
        least one vertex in C{vertices}

        @param vertices: The vertices for which containing hyperedges are sought
        @type vertices: Sequence
        @return: The set of hyperedges each of which contain at least one vertex in C{vertices}
        @rtype: Set
        @raise KeyError: If C{vertices} contains a vertex not in the (hyper)graph
        """
        hyperedges = set()
        for vertex in vertices:
            hyperedges.update(self._star[vertex])
        return hyperedges


    def two_section(self):
        """Return the 2-section of a hypergraph

        This graph contains an undirected edge for any pair of distinct vertices
        which are both members of a hyperedge.
        @return: The interaction graph
        @rtype: L{UGraph}
        """
        import Graphs
        ig = Graphs.UGraph()
        for hyperedge in self._hyperedges:
            ig.add_clique(hyperedge)
        return ig

    def vertices(self):
        """Return the vertices (base set) of the hypergraph

        @return: The vertices (base set) of the hypergraph
        @rtype: Set
        """
        return set(self._star)

    def would_be_redundant(self,hyperedge):
        """Whether C{hyperedge} would be redundant if it
        were added to the hypergraph
        
        If C{hyperedge} is already in the hypergraph it is considered
        that it would be redundant if added.

        @param hyperedge: A potential hyperedge
        @type hyperedge: Iterable
        """
        if not hyperedge:
            if self._hyperedges:
                return True
            else:
                return False
        for vertex in hyperedge:
            try:
                try:
                    cs.intersection_update(
                        self._star[vertex])
                    if not cs:
                        return False
                except NameError:
                    cs = self._star[vertex].copy()
            except KeyError:
                return False
        return True


    def _add_hyperedge(self,hyperedge):
        """add a hyperedge known to be new"""
        self._hyperedges.add(hyperedge)
        for vertex in hyperedge:
            try:
                self._star[vertex].add(hyperedge)
            except KeyError: # new vertex
                self._star[vertex] = set([hyperedge])

class SimpleHypergraph(Hypergraph):
    """A simple hypergraph has no repeated hyperedges and thus is a B{set} of hyperedges.
    """

    def __init__(self,hypergraph=(),check=True,modify=False):
        """Initialise a simple hypergraph

        @param hypergraph: Hypergraph/hyperedges used to generate new simple hypergraph 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param check: In the case that C{modify=False}, whether to check that
        C{hypergraph} is simple
        @type check: Boolean
        @param modify: Whether C{hypergraph} is to modified to be made simple
        @type modify: Boolean
        @raise TypeError: If C{hyperedges} is not of the right form.
        @raise ValueError: If C{hyperedges} contains repeats.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if hasattr(hypergraph,'_extras'):
            if modify:
                del hypergraph._extras
            elif check:
                raise ValueError(
                    "Can't make a simple hypergraph because these hyperedges: %s are repeated this many times: %s, respectively" %
                    (self._extras.keys(),self._extras.values()))
        self.__dict__ = hypergraph.__dict__
         
    def __str__(self):
        """Return pretty representation of a hypergraph

        @return: Pretty representation of a hypergraph
        @rtype: String
        """
        return '{ %s }' % ', '.join(
            [pretty_str_set(hyperedge) for hyperedge in self._hyperedges]
            )

    def add_hyperedge(self,hyperedge):
        """Add a hyperedge to a simple hypergraph

        @param hyperedge: The vertices in the hyperedge
        @type hyperedge: Iterable
        @raise ValueError: If C{hyperedge} already in hypergraph
        """
        hyperedge = frozenset(hyperedge)
        if hyperedge in self._hyperedges:
            raise ValueError('%s already in hypergraph' % hyperedge)
        else:
            Hypergraph._add_hyperedge(self,hyperedge)

class ReducedHypergraph(SimpleHypergraph):
    """Hypergraphs without redundant hyperedges
    """

    def __init__(self,hypergraph=(),check=True,modify=False,trace=False):
        """Initialise a reduced hypergraph

        If C{hypergraph} is indeed a hypergraph then the new
        L{ReducedHypergraph} object has identical attributes to
        C{hypergraph}.

        If C{hypergraph} is not a hypergraph then it is assumed to be
        a collection of hyperedges, a C{Hypergraph} object is created
        from these hyperedges as the first step of construction and
        execution proceeds exactly as if this C{Hypergraph} object had
        been the original value of C{hypergraph}. The default value
        for C{hypergraph} constructs is an empty C{ReducedHypergraph}).

        @param hypergraph: Hypergraph/hyperedges used to generate new reduced hypergraph 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param check: In the case that C{modify=False}, whether to check that
        C{hypergraph} is reduced
        @type check: Boolean
        @param modify: Whether C{hypergraph} is to modified to be made reduced
        @type modify: Boolean
        @param trace: In the case where C{modify=True} whether a mapping
        from removed redundant hyperedges to the set of hyperedges containing them
        should be created. If so, it is stored in a C{trace} attribute of C{self}
        @type trace: Boolean
        @raise RedundancyError: If C{check=True,modify=False} and C{hypergraph} is not reduced.
        @raise TypeError: If C{hypergraph} is not a hypergraph or an iterable of
        hyperedges.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if modify:
            reds = hypergraph.redundant_hyperedges()
            for red in reds:
                hypergraph.remove_hyperedge(red)
            if trace:
                hypergraph.trace = reds
        elif check and not hypergraph.is_reduced():
            for h in hypergraph:
                if hypergraph.is_redundant(h):
                    break
            raise RedundancyError("%s is not reduced due to %s" % (hypergraph,h))
        self.__dict__ = hypergraph.__dict__

    
    def add_hyperedge(self,hyperedge,check=True):
        """Add a hyperedge 

        @param hyperedge: The vertices in the hyperedge
        @type hyperedge: Frozenset
        @param check: Whether to check that adding C{hyperedge} does not introduce redundancy
        @type check: Boolean
        @raise RedundancyError: If C{check} is True and adding C{hyperedge} would render the
        hypergraph no longer reduced.
        """
        if check and self.makes_unreduced(hyperedge):
            raise RedundancyError("Adding %s would make %s no longer reduced" %
                                  (pretty_str_set(hyperedge),self))
        Hypergraph.add_hyperedge(self,hyperedge)

    def is_graphical(self):
        return self == self.two_section().hypergraph()

#     def red(self):
#         return

#     def reduced(self):
#         return self.copy()

    def is_reduced(self):
        return True

    def reduction(self):
        """Apply the 'Reduction Algorithm' of Tarjan and Yannakakis

        SIAM J Computing 13 (1984) 566-579
        On return C{self} will be the I{reduction} of the original C{self}.
        Despite the name, this is not the same as reducing the hypergraph by
        removing redundant hyperedges.
        @return: The removed hyperedges
        @rtype: List
        """
        removed = []
        while True:
            for vertex, hyperedges in self._star.items():
                if len(hyperedges) == 1:
                    hyperedge = member(hyperedges) # only 1 member
                    self.remove_hyperedge(hyperedge)
                    new_hyperedge = hyperedge - set([vertex])
                    try:
                        self.add_hyperedge(new_hyperedge)
                    except RedundancyError:
                        removed.append(new_hyperedge)
                    break
            else:
                return removed

    def redundant_hyperedges(self):
        return {}


class GraphicalHypergraph(Hypergraph):
    """For a hypergraph H to be I{graphical}, the hyperedges of red(H) must be the cliques of
    an (undirected) graph.
    """
    def __init__(self,hypergraph=(),check=True):
        """Initialise a graphical hypergraph

        If C{hypergraph} is indeed a hypergraph then the new
        L{GraphicalHypergraph} object has identical attributes to
        C{hypergraph}.

        If C{hypergraph} is not a hypergraph then it is assumed to be
        a collection of hyperedges, a C{Hypergraph} object is created
        from these hyperedges as the first step of construction and
        execution proceeds exactly as if this C{Hypergraph} object had
        been the original value of C{hypergraph}. The default value
        for C{hypergraph} constructs is an empty C{GraphicalHypergraph}).

        @param hypergraph: Hypergraph/hyperedges used to generate new graphical hypergraph 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param check: Whether to check that C{hypergraph} is reduced and graphical
        @type check: Boolean
        @raise GraphicalityError: If C{hypergraph} is not graphical.
        @raise TypeError: If C{hypergraph} is not a hypergraph or an iterable of
        hyperedges.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if check and not hypergraph.is_graphical():
                raise GraphicalityError("%s is not graphical" % hypergraph)
        self.__dict__ = hypergraph.__dict__

    def is_graphical(self):
        return True

        
class ReducedGraphicalHypergraph(GraphicalHypergraph,ReducedHypergraph):
    """For a hypergraph H to be I{graphical}, the hyperedges of red(H) must be the cliques of
    an (undirected) graph.

    A ReducedGraphicalHypergraph object is both reduced and graphical so its
    hyperedges are the cliques of an (undirected) graph.
    """
    def __init__(self,hypergraph=(),check=True):
        """Initialise a graphical reduced hypergraph

        If C{hypergraph} is indeed a hypergraph then the new
        L{ReducedGraphicalHypergraph} object has identical attributes to
        C{hypergraph}.

        If C{hypergraph} is not a hypergraph then it is assumed to be
        a collection of hyperedges, a C{Hypergraph} object is created
        from these hyperedges as the first step of construction and
        execution proceeds exactly as if this C{Hypergraph} object had
        been the original value of C{hypergraph}. The default value
        for C{hypergraph} constructs is an empty C{ReducedGraphicalHypergraph}).

        @param hypergraph: Hypergraph/hyperedges used to generate new reduced
        graphical hypergraph 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param check: Whether to check that C{hypergraph} is reduced and graphical
        @type check: Boolean
        @raise RedundancyError: If C{check=True} and C{hypergraph} is not reduced.
        @raise GraphicalityError: If C{check=True} and C{hypergraph} is not graphical.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if check:
            if not hypergraph.is_reduced():
                raise RedundancyError("%s is not reduced" % hypergraph)
            if not hypergraph.is_graphical():
                raise GraphicalityError("%s is not graphical" % hypergraph)
        self.__dict__ = hypergraph.__dict__



class DecomposableHypergraph(GraphicalHypergraph):
    """For a graphical hypergraph to be I{decomposable}, the
    hyperedges of red(H) must be the cliques of some (undirected)
    B{decomposable} graph.
    
    """
    def __init__(self,hypergraph=(),check=True,modify=False,
                 trace=False,elimination_order=None):
        """Initialise a decomposable hypergraph
        
        If C{hypergraph} is indeed a hypergraph then the new
        L{DecomposableHypergraph} object has identical attributes to
        C{hypergraph}.

        If C{hypergraph} is not a hypergraph then it is assumed to be
        a collection of hyperedges, a C{Hypergraph} object is created
        from these hyperedges as the first step of construction and
        execution proceeds exactly as if this C{Hypergraph} object had
        been the original value of C{hypergraph}. The default value
        for C{hypergraph} constructs is an empty C{DecomposableHypergraph}).

        @param hypergraph: Hypergraph/hyperedges used to generate new
        decomposable hypergraph 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param check: In the case that C{modify=False}, whether to check that
        C{hypergraph} is decomposable
        @type check: Boolean
        @param modify: Whether C{hypergraph} is to be modified to be made decomposable
        @type modify: Boolean
        @param trace: In the case where C{modify=True} whether a mapping
        from each hyperedge to the new hyperedge into which it has been merged
        should be created. If so, it is stored in a C{trace} attribute of C{self}
        @type trace: Boolean
        @param elimination_order: If supplied
        and C{modify=True}, the elimination order to use to make the C{hypergraph}
        decomposable. (If not supplied maximum cardinality search is used to generate an order.)
        @type elimination_order: Sequence
        @raise DecomposabilityError: If C{check=True,modify=False} and C{hypergraph}
        is not decomposable.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if check or modify:
            JoinForest(hypergraph,modify,trace,elimination_order)
            del hypergraph._uforest
        self.__dict__ = hypergraph.__dict__

    def is_decomposable(self):
        return True
        
     # def __init__(self,hypergraph,elimination_ordering=None,force=False):
#          """Construct a decomposable hypergraph from an existing hypergraph

#          If C{force} is C{False} then the assumption is that C{hypergraph}
#          is decomposable and it will be the constructed decomposable hypergraph.
#          If C{force} is C{True} then a new decomposable hypergraph is created by merging
#          hyperedges in C{hypergraph}

#          If C{elimination_ordering} is given it is ...
#          """
#          self._hyperedges = set()
#          self._star = {}
#          if elimination_ordering is None:
#              if force:
#                  elimination_ordering = self.find_elimination_ordering_force(hypergraph,[])
#              else:
#                  elimination_ordering = self.find_elimnation_ordering_no_force(hypergraph,[])
#          else:
#              if force:
#                  self.use_ordering_force(hypergraph,elimination_ordering)
#              else:
#                  self.use_ordering_no_force(hypergraph,elimination_ordering)
#          self._elimination_ordering = tuple(elimination_ordering)

#      def find_elimination_ordering(self,hypergraph,order_prefix,force):
#          pass

#      def use_ordering_no_force(self,hypergraph,elimination_ordering):
#          if not elimination_ordering:
#              return
#          vertex = elimination_ordering.pop(0)
#          vertex_hyperedges = hypergraph.hyperedges_containing_vertex(vertex) 
#          if len(vertex_hyperedges) != 1:
#              raise ValueError("Vertex %s in more than one hyperedge in %s." % vertex, hypergraph)
#          hyperedge = member(vertex_hyperedges)
#          hypergraph.remove_hyperedge(hyperedge)
#          new_hyperege = hyperedge - set([vertex])
#          if not hypergraph.would_be_redundant(new_hyperedge):
#              hypergraph.add_hyperedge(new_hyperedge)
#          self.use_ordering_no_force(self,hypergraph,elimination_ordering)

class ReducedDecomposableHypergraph(DecomposableHypergraph,ReducedGraphicalHypergraph):
    """For a graphical hypergraph to be I{decomposable}, and
    I{reduced} the hyperedges of H must be the cliques of some
    (undirected) B{decomposable} graph.
    """


class JoinForest(DecomposableHypergraph):
    """A L{DecomposableHypergraph} whose hyperedges are the vertices of a join forest

    @ivar _uforest: The join forest itself
    @type _uforest: L{Graphs.UForest}
    #@ivar _elimination_ordering: An elimination ordering consistent with the
    #hypergraph's L{_uforest}
    #@type _elimination_ordering: Tuple
    #@ivar destination: Maps hyperedges in the hypergraph from which the instance was
    #created to hyperedges in the instance. Can be deleted without altering the instance.
    #@type destination: Dictionary
    #@ivar edges: The edges of the instance's L{_uforest} when the instance was
    #created. Each edge is a frozenset
    #with the two connected cliques as members. Can be deleted without altering the instance.
    #@type edges: List
    """

    def __init__(self,hypergraph=(),modify=False,
                 trace=False,elimination_order=None):
        """Construct join forest from a hypergraph

        If C{hypergraph} is indeed a hypergraph then the new
        L{JoinForest} object has identical attributes to
        C{hypergraph} (plus a new one of its own).

        If C{hypergraph} is not a hypergraph then it is assumed to be
        a collection of hyperedges, a C{Hypergraph} object is created
        from these hyperedges as the first step of construction and
        execution proceeds exactly as if this C{Hypergraph} object had
        been the original value of C{hypergraph}. The default value
        for C{hypergraph} constructs an empty C{JoinForest}).

        @param hypergraph: Hypergraph/hyperedges used to generate new
        join forest 
        @type hypergraph: L{Hypergraph} or iterable of iterables
        @param modify: Whether C{hypergraph} is to modified to be made decomposable
        @type modify: Boolean
        @param trace: In the case where C{modify=True} whether a mapping
        from each hyperedge to the new hyperedge into which it has been merged
        should be created. If so, it is stored in a C{trace} attribute of C{self}
        @type trace: Boolean
        @param elimination_order: If supplied
        and C{modify=True}, the elimination order to use to make the C{hypergraph}
        decomposable. (If not supplied maximum cardinality search is used to generate an order.)
        @type elimination_order: Sequence
        @raise DecomposabilityError: If C{modify=False} and C{hypergraph} is not decomposable.
        """
        if not isinstance(hypergraph,Hypergraph):
            hypergraph = Hypergraph(hypergraph)
        if modify:
            # would be better to make join forest here
            dg, trace_dict = hypergraph.make_decomposable2(elimination_order)
            hypergraph.__dict__ = dg.__dict__
            if trace:
                hypergraph.trace = trace_dict
        hypergraph._uforest = hypergraph.join_forest()
        self.__dict__ = hypergraph.__dict__

    def __str__(self):
        forest_str = str(self._uforest)
        for hyperedge in self._hyperedges:
            forest_str = forest_str.replace(str(hyperedge),pretty_str_set(hyperedge))
        return '%s\n%s' % (Hypergraph.__str__(self),forest_str)

    def copy(self):
        cp = JoinForest()
        cp._hyperedges.update(self._hyperedges)
        for vertex, hyperedges in self._star.items():
            cp._star[vertex] = hyperedges.copy()
        cp._uforest = self._uforest.copy()
        return cp

    def join_forest(self):
        return self._uforest
    
# self._hyperedges = hypergraph._hyperedges
#         self._star = hypergraph._star
#         self._elimination_ordering = hypergraph._elimination_ordering
#         join_forest = UForest(self._hyperedges)
#         tmp_hypergraph = hypergraph.copy()
#         orig = {}
#         for hyperedge in hypergraph:
#             orig[hyperedge] = hyperedge
#         elimination_ordering = list(hypergraph._elimination_ordering)
#         while elimination_ordering:
#             vertex_hyperedge = member(tmp_hypergraph.hyperedges_containing_vertex(vertex))
#             tmp_hypergraph.remove_hyperedge(vertex_hyperedge)
#             new_hyperedge = vertex_hyperedge - set([vertex])
#             superset = hypergraph.superset(new_hyperedge)
#             if superset:
#                 join_forest.put_line(orig[vertex_hyperedge],orig[superset])
#             else:
#                 orig[new_hyperedge] = orig[vertex_hyperedge]
#                 tmp_hypergraph.add_hyperedge(new_hyperedge)
#             del orig[vertex_hyperedge]
#         self._uforest = join_forest

        
#     def __init__(self,hypergraph,variables):
#         """
#         Construct a join forest from an arbitrary hypergraph
#         and an elimination ordering
        
#         @param hypergraph: Generating hypergraph
#         @type hypergraph: L{Hypergraph}
#         @param variables: Elimination ordering
#         @type variables: Sequence
#         @raise ValueError: If C{variables} is not an ordering of the vertices in C{hypergraph}
#         """
#         if sorted(variables) != sorted(hypergraph.vertices()):
#             raise ValueError("%s not an ordering of %s" % (variables,hypergraph.vertices()))
#         DecomposableHypergraph.__init__(self)
#         uf = UForest()
#         frm = {}
#         destination = {}
#         edges = []
#         for variable in variables:
#             clique = set()
#             hyperedges = hypergraph.hyperedges_containing_vertex(variable)
#             for hyperedge in hyperedges:
#                 clique.update(hyperedge)
#                 hypergraph.remove_hyperedge(hyperedge)

#             messages = hyperedges.intersection(frm)
#             container = None
#             for msg in messages:
#                 if clique <= msg:
#                     clique = frm[msg][0]
#                     container = msg
#                     break
#             if container is None:
#                 clique = frozenset(clique)
#                 self.add_hyperedge(clique,check=False)
#                 uf.add_vertex(clique)
#             else:
#                 messages.remove(container)

#             hyperedges.difference_update(messages)
#             for original_hyperedge in hyperedges:
#                 destination[original_hyperedge] = clique
             
#             for msg in messages:
#                 for sender in frm[msg]:
#                     uf.put_line(sender,clique)
#                     edges.append(frozenset([sender,clique]))
#                 del frm[msg]

#             new_msg = clique - set([variable])
#             if new_msg:
#                 hypergraph.add_hyperedge(new_msg)
#                 try:
#                     frm[new_msg].append(clique)
#                 except KeyError:
#                     frm[new_msg] = [clique]
#         self._uforest = uf
#         self._elimination_ordering = tuple(variables)
#         self.destination = destination
#         self.edges = edges

    def clique(self):
        """Return an arbitrary clique

        @return: An arbitrary clique
        @rtype: Frozenset
        """
        for clique in self._hyperedges:
            return clique

    def clique_neighbours(self,clique,banned=emptyset):
        """Return neighbours of a clique in the join forest

        Excludes the cliques in C{banned}
        @param clique: Cliques whose neighbours are sought
        @type clique: Frozenset
        @param banned: Neighbours to exclude from result
        @type banned: set or frozenset
        """
        return self._uforest.neighbours(clique) - banned

    def perfect_sequence(self,root=None):
        """Return an ordering of the cliques obeying the runaning intersection
        property

        @param root: A root hyperedge for the ordering; it will be the
        first element of the returned ordering. If not supplied, it is chosen
        arbitrarily.
        @type root: An iterable
        @return: An ordering of the cliques obeying the running intersection
        property
        @rtype: List
        @raise ValueError: If C{root} is supplied but not in join forest.
        """
        unnumbered = self._hyperedges.copy()
        sequence = []
        if root is not None:
            root = frozenset(root)
            if root not in unnumbered:
                raise ValueError("Root %s not in join forest")
        while unnumbered:
            if root is None:
                root = unnumbered.pop()
            else:
                unnumbered.remove(root)
            frontier = set([root])
            sequence.append(root)
            while frontier:
                clique = frontier.pop()
                for nbr in self._uforest.neighbours(clique) & unnumbered:
                    sequence.append(nbr)
                    unnumbered.remove(nbr)
                    frontier.add(nbr)
            root = None
        return sequence

class ReducedJoinForest(JoinForest,ReducedDecomposableHypergraph):
    """A L{ReducedDecomposableHypergraph} whose hyperedges are the vertices of a join forest
    """
    def __str__(self):
        forest_str = str(self._uforest)
        for hyperedge in self._hyperedges:
            forest_str = forest_str.replace(str(hyperedge),pretty_str_set(hyperedge))
        return '%s\n%s' % (ReducedHypergraph.__str__(self),forest_str)


            
    # def grahams2(self):
#         """Run Graham's algorithm on a hypergraph

#         If the hypergraph is decomposable then upon return C{self} is either
#         empty or contains a single empty hyperedge.
#         """
#         if self.is_empty():
#             return self
        
#         self.red()

#         cardinality = {}
#         sets = [set()]
#         max = 0
#         for vertex, star in self._star.items():
#             card = len(star) - 1
#             cardinality[vertex] = card
#             if card > max:
#                 for i in range(card-max):
#                     sets.append(set())
#                 max = card
#             sets[card].add(vertex)

#         while sets[0] != set():
#             for vertex in sets[0]:
#                 self.remove_vertex(vertex)
#             sets[0] = set()
#             for redundant_hyperedge in self.red():
#                 for vertex in redundant_hyperedge:
#                     card = cardinality[vertex]
#                     sets[card].remove(vertex)
#                     card -= 1
#                     cardinality[vertex] = card
#                     sets[card].add(vertex)
#         return self

#     def is_decomposable(self):
#         """Return whether the hypergraph is decomposable

#         @return: Whether the hypergraph is decomposable
#         @rtype: Boolean
#         @todo: Implement efficiently
#         """
#         if self._hyperedges == set():
#             return True
#         try:
#             ReducedHyperGraph(self,modify=True).join_forest()
#             return True
#         except DecomposabilityError:
#             return False

#     def join_forest(self):
#         """Return a join forest graph, assuming decomposability

#         Uses L{maximum_cardinality_search}. See that method for bibliographical
#         references.
#         @return: A join forest graph
#         @rtype: L{UForest}
#         @raise RedundancyError: If hypergraph is not reduced
#         @raise DecomposabilityError: If hypergraph is reduced but not decomposable
#         """
#         alpha, beta, gamma, inv_gamma, r = self.maximum_cardinality_search()
#         index = {}
#         for i, hyperedge in enumerate(r):
#             for other in inv_gamma[i]:
#                 for vertex in other:
#                     if beta[vertex] < i and vertex not in hyperedge:
#                         raise DecomposabilityError("%s is not decomposable" % self)
#         join_forest = UForest(self._hyperedges)
#         for hyperedge in self._hyperedges:
#             try:
#                 join_forest.put_line(hyperedge,r[gamma[hyperedge]])
#             except KeyError:
#                 pass
#         return join_forest

#     def make_reduced(self):
#         """Reduce the hypergraph and make it
#         a L{ReducedHyperGraph} object

#         @return: Set of superset hyperedges for each redundant hyperedge
#         @rtype: Dictionary
#         """
#         supersets = self.redundant_hyperedges()
#         for redundant_hyperedge in supersets:
#             self.remove_hyperedge(redundant_hyperedge)
#         self.__class__ = ReducedHyperGraph
#         return supersets

#     def maximum_cardinality_search(self,choose=None):
#         """Return maximum cardinality search

#         This is 'Restricted Maximum Cardinality Search on Hypergraphs' as presented in the
#         reference below. Note there are two errors in the algorithm as presented there: '.. i:=i+1'
#         should be '..i:=i-1' on line 9. Also the 'j:=j+1' statement near the end needs to be moved
#         into the loop over vertices to ensure it is big enough.

#         Ref::

#          @Article{tarjan84:_simpl,
#          author = 	 {Robert E. Tarjan and Mihalis Yannakakis},
#          title = 	 {Simple linear-time algorithms to test chordality of graphs, test acyclicity of hypergraphs, and selectively reduce acyclic hypergraphs}, 
#          journal = 	 {{SIAM} Journal of Computing},
#          year = 	 1984,
#          volume =	 13,
#          number =	 3,
#          pages =	 {566--579},
#          month =	 {August}

#         Returns C{(alpha, beta, gamma, inv_gamma, r)} where
#          - C{alpha} is a dictionary mapping each vertex to its index in the generated vertex ordering.
#            Note that the first vertex to be numbered comes I{last} in this ordering.
#            Note that here the lowest index is 0, not 1 as in the original presentation
#          - C{beta} is a dictionary mapping each hyperedge to its index in generated hyperedge ordering.
#            Note that the first hyperedge to be numbered comes I{first} in this ordering.
#            C{beta} also maps each I{vertex} C{v} to
#            min {beta[hyperedge] : hyperedge gets 'selected' and v in hyperedge}
#            Note that beta[v] < beta[w] implies alpha[v] > alpha[w]
#            Note that here the lowest index is 0, not 1 as in the original presentation
#          - C{gamma} is a dictionary mapping hyperedges as follows:
#            - If a hyperedge is not selected then gamma[hyperedge] = max{beta[v]:v in hyperedge}
#            - If a hyperedge is selected then
#              gamma[hyperedge] = max{beta[v]:v in hyperedge and beta[v] < beta[hyperedge]}
#              if this is defined
#          - C{inv_gamma} is just a list representing the inverse mapping to C{gamma}
#          - C{r} is the generated hyperedge ordering (inverse of C{beta})

#         @param choose: Function for choosing between hyperedges with an equally high number of numbered vertices.
#         If unsupplied the choice is arbitrary.
#         @type choose: A function which removes and returns an element from a set.
#         @return: C{(alpha, beta, gamma, inv_gamma, r)} where these have the same meanings as in reference above.
#         C{inv_gamma} is just the mapping gamma inverted.
#         @rtype: Tuple
#         @raise RedundancyError: If hypergraph is not reduced
#         """
#         if not self.is_reduced():
#             raise RedundancyError("%s is not reduced" % self)

#         if choose is None:
#             def choose(set): return set.pop()
            
#         n = len(self._star)
#         sets = []
#         for i in range(n+1):
#             sets.append(set())
#         cardinality = {}
#         inv_gamma = []
#         for hyperedge in self._hyperedges:
#             cardinality[hyperedge] = 0
#             inv_gamma.append(set())
#         sets[0] |= self._hyperedges
#         alpha, beta, gamma = {}, {}, {}
#         i, j, k = n, 0, -1
#         r = []

#         while j >= 0 and sets[j]:  # 'and sets[j] for empty hypergraphs'
#             hyperedge = choose(sets[j])
#             k += 1

#             beta[hyperedge] = k
#             r.append(hyperedge)
#             del cardinality[hyperedge]

#             for vertex in hyperedge.difference(alpha):
#                 i -= 1

#                 alpha[vertex] = i
#                 beta[vertex] = k

#                 for other_hyperedge in self._star[vertex].intersection(cardinality):
#                     gamma[other_hyperedge] = k
#                     inv_gamma[k].add(other_hyperedge)
                    
#                     card = cardinality[other_hyperedge]
#                     sets[card].remove(other_hyperedge)
#                     card += 1
#                     cardinality[other_hyperedge] = card
                    
#                     if card < len(other_hyperedge):
#                         sets[card].add(other_hyperedge)
#                     else:
#                         del cardinality[other_hyperedge]
#                 j += 1
#             while j >= 0 and not sets[j]:
#                 j -= 1
#         return alpha, beta, gamma, inv_gamma, r

                    # w < v, v < z => w < z
                    # same as v < z, z < w => v < w
                    #clause = (ord_atom[v,w],-ord_atom[v,z],ord_atom[w,z])
                    #hard_clauses.append(clause)
                    #if comments:
                    #    print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
                    #    clause, w,v,v,z,w,z)
                    # w < z, z < v => w < v
                    # same as
                    #  v < w, w < z => v < z
                    #clause = (-ord_atom[w,z],ord_atom[v,z],-ord_atom[v,w])
                    #hard_clauses.append(clause)
                    #if comments:
                    #    print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
                    #    clause, w,v,v,z,w,z)
                    # z < v, v < w => z < w
                    # same as v < w, w < z => v < z
                    #clause = (ord_atom[v,z],-ord_atom[v,w],-ord_atom[w,z])
                    #hard_clauses.append(clause)
                    #if comments:
                    #    print >> fobj, 'c Clause %s since %s < %s & %s < %s => %s < %s' % (
                    #    clause, w,v,v,z,w,z)
                    # z < w, w < v => z < v
                    #clause = (ord_atom[w,z],ord_atom[v,w],-ord_atom[v,z])
