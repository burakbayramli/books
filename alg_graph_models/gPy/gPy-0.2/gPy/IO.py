"""
Functions for IO of models and data

@var _version: Version of this module
@type _version: String
"""

import urllib
import re
import Tkinter
import random

_version = '$Id: IO.py,v 1.5 2008/10/07 09:10:27 jc Exp $'


def read_bif(fobj):
    """Read in a BN in BIF format

    Returns 3 dictionaries,
    The first maps each variable to a tuple of its values.
    The second maps each variable to a tuple of its parents.
    The third maps each instantiation of its parents (order as for tuple in 2nd dictionary)
    to a list of conditional probs (order as for tuple in 1st dictionary)

    @param fobj: BIF file object
    @type fobj: File
    @return: 3 dictionaries
    @rtype: Tuple
    """
    domain = {}
    cpts = {}
    parents = {}

    # filter out comments and properties
    prop_re = re.compile(r'^\s*property.*')
    comment_re = re.compile(r'//.*')
    bif = ''
    for line in fobj:
        if prop_re.search(line):
            continue
        line = comment_re.sub('',line)
        bif += line
    var_pattern = re.compile(r'^variable\s+(\w+)\s+\{\s+type\s+(\w+)\s*\[\s*(\d+)\s*\]\s*=?\s*\{([^}]+)\}',re.MULTILINE)
    ws_pattern = re.compile(r'\s+')
    for (var,type,numvals,vals) in var_pattern.findall(bif):
        if type != 'discrete':
            raise ValueError('%s is not discrete, gPy can only handle discrete variables' % var)
        valset = tuple(ws_pattern.sub('',vals).split(','))
        if len(valset) != len(set(valset)):
            raise ValueError("%s has values %s, can't be right!" % (var,valset))
        numvals = int(numvals)
        if len(valset) != numvals:
            raise ValueError('%s has %s as values, but should have %d values' % (var,vals,numvals))
        domain[var] = valset
    orphan_pattern = re.compile(r'^probability\s+\(\s*(\w+)\s*\)\s*\{\s*table([^;]+);\s*\}',re.MULTILINE)
    for (var,probs) in orphan_pattern.findall(bif):
        parents[var] = ()
        probsfloats = [float(pr) for pr in ws_pattern.sub('',probs).split(',')]
        if len(probsfloats) != len(domain[var]):
            raise ValueError('%s has %d values but these probabilities: %s' % (var,len(domain[var]),probs))
        cpts[var] = {():probsfloats}
    child_pattern = re.compile(r'^probability\s+\(\s*(\w+)\s*\|([^)]+)\)\s*\{([^}]+)\}',re.MULTILINE)
    row_pattern = re.compile(r'\s*\(([^)]+)\)([^;]+);')
    for (var,pas,rows) in child_pattern.findall(bif):
        parents[var] = tuple(ws_pattern.sub('',pas).split(','))
        cpts[var] = {}
        cptdict = cpts[var]
        for (inst,probs) in row_pattern.findall(rows):
            cptdict[tuple(ws_pattern.sub('',inst).split(','))] = [float(pr) for pr in ws_pattern.sub('',probs).split(',')]
    return domain, parents, cpts


def csv2db(csv,db_file):
    """Create a sqlite database from a CSV file
    """
    pass
        
def read_dimacs(fobj):
    """Read a graph in DIMACS format

    Only vertices appearing in an edge are considered.
    All vertices assumed to be integers

    @param fobj: DIMACS file object
    @type fobj: File
    @return: Vertices, Edges of the graph
    @rtype: Tuple
    """
    edges = []
    vertices = set()
    for line in fobj:
        if line[0] == 'e':
            [e,v,w] = line.rstrip().split()
            v = int(v)
            w = int(w)
            vertices.add(v)
            vertices.add(w)
            edges.append((v,w))
    return vertices, edges
                        


def read_csv(fobj,sep=','):
    """Construct a list of records from a CSV file
    
    The CSV file must be have 3 sections.

    Section 1 has lines of the form: C{variable:value1,value2,..}. There is one line
    for each variable
    
    Section 2 is a single line of C{sep} separated variables. If the M{j}th one
    of these is I{varname} then the M{j}th field of each record is a value for I{varname}.
    
    Section 3 consists of records, one per line with C{sep} separating the fields.
    Either each record has an extra 'count' field or none do.

    No further lines are read after an empty line (so trailing empty lines
    do not cause an IOError).

    Here's part of an acceptable CSV file (where the optional extra count field I{is} present)::
    
     A:N,Y
     S:0,1
     T:0,1
     L:0,1
     B:0,1
     E:0,1
     D:0,1
     X:0,1
     A,S,T,L,B,E,D,X
     N,1,0,1,1,1,1,1,12
     N,1,0,0,0,0,0,0,66


    @param fobj: CSV file object (NOT the file I{name})
    @type fobj: File
    @param sep: The field separator in C{fobj}.
    @type sep: String
    @return: C{(header, values, variables, records)} where:
    
     0. C{header} is a list of the variables in the data in the order they are
    given in the original data file. C{[A,S,T,L,B,E,D,X]} in the example above.

     1. C{values} is a dictionary mapping variables to their values, where
    these values are a list. C{{'A':['N','Y'],'S':['0','1']...}} in the example above.

     2. C{variables} is an ordered list of the variables in the data. C{[A,B,D,E,L,T,S,X]}
     in the example above.

     3. C{records} is a list of records, each record is a tuple of integers. For each tuple the M{j}th element
     is the index of the value of the M{j}th variable of C{variables} found in that record.
     The final integer is a count of how often the record appeared.

    @todo: It would be nice to allow this to be an iterator where possible.
    @rtype: Tuple
    @raise IOError: If any record has the wrong number of fields
    """

    # read in variable descriptors
    values = {}
    header = fobj.readline().rstrip().split(':')
    linenum = 1
    while len(header) == 2:
        values[header[0]] = header[1].split(sep)
        header = fobj.readline().rstrip().split(':')
        linenum += 1

    # read in field header
    header = header[0].rstrip().split(sep)
    # and first record
    line = fobj.readline()
    inst = line.rstrip().split(sep)
    linenum += 1

    # determine whether records have a count field appended
    record_length = len(inst)
    if record_length == len(header):
        counts = False
        record_dict = {}
    elif record_length == len(header) + 1:
        counts = True
    else:
        raise IOError("Length of header incompatible with length of first record")

    # set up
    variables= header[:]
    variables.sort()
    indices = [header.index(variable) for variable in variables]
    val_index = []
    for variable in variables:
        tmp = {}
        for i, val in enumerate(values[variable]):
            if val in tmp:
                raise IOError("%s has %s as a value more than once" % (variable,val))
            tmp[val] = i
        val_index.append(tmp)
    records = []
    field_indices = range(len(variables))

    while line != '\n' and line != '':
        # check inst is OK
        #print line, inst, len(line)
        if len(inst) != record_length:
            raise IOError("%d fields on line %d, should be %d" %
                          (len(inst),linenum,record_length))
        #make record
        record = []
        for j in field_indices:
            record.append(val_index[j][inst[indices[j]]])
        if counts:
            record.append(int(inst[-1]))
        record = tuple(record)
        #store record
        if counts:
            records.append(record)
        else:
            try:
                record_dict[record] += 1
            except KeyError:
                record_dict[record] = 1
        # read in next line and parse it
        line = fobj.readline()
        linenum += 1
        inst = line.rstrip().split(sep)
    if not counts:
        for record, count in record_dict.iteritems():
            records.append(record+((count),))
    fobj.close()
    return header, values, variables, records


def read_norsyslib(net,get_positions=False):
    return read_dnet(urllib.urlopen('http://www.norsys.com/netlib/%s.dnet' % net.upper()),
                     get_positions)

def read_dnet(fobj,get_positions=False):
    """Return CPTs of a Bayesian net in
    Netica 'dnet' format

    Does not work if inheritance is being used in the dnet file!
    @param fobj: File or name of file containing the input
    @type fobj: File or String
    @return: (C{values},C{named_cpts}) where:

     0. C{values} is a dictionary mapping variables to their values, where
    these values are a list. For example, C{{'Tuberculosis':['Present','Absent'], ...}}

     1. C{named_cpts} is a dictionary mapping variables to a tuple of 0) their parents
    and 1) the data for that CPT, where data is correctly ordered for a factor.

    If C{get_positions} is True, then a dictionary mapping variables to grid positions
    is also obtained.
    
    @rtype: Tuple
    """
    if isinstance(fobj,str):
        fobj = open(fobj)
    node_re = re.compile(r'^node (\S+) {')
    states_re = re.compile(r'states = \(([^)]+)\)')
    parents_re = re.compile(r'parents = \(([^)]*)\)')
    tuple_re = re.compile(r'\([^)(]+\)')
    if get_positions:
        coords = {}
        coords_re = re.compile(r'center = \((\d+)\D+(\d+)')
    
    named_cpts = {}
    values = {}

    while True:
        node_match = node_re.search(fobj.next())
        while not node_match:
            try:
                line = fobj.next()
            except StopIteration:
                if get_positions:
                    return (values, named_cpts, coords)
                else:
                    return (values, named_cpts)
            node_match = node_re.search(line)
        node = node_match.group(1)

        # skip over any other sort of node        
        if 'kind = NATURE' not in fobj.next():
            continue
        #if 'discrete = TRUE' not in fobj.next():
        #    continue
        # car diagnosis has a non-discrete variable which is discrete!
        fobj.next()
        if 'chance = CHANCE' not in fobj.next():
            continue
        states = tuple(states_re.search(fobj.next()).group(1).split(', '))
        values[node] = states  
        parent_str = parents_re.search(fobj.next()).group(1)
        if parent_str:
            parents = parent_str.split(', ')
        else:
            parents = []
        # construct parent configs in the order they will be
        # in the lines in the dnet file
        parentconfigs = [[]]
        for parent in parents:
            tmp = []
            for parentconfig in parentconfigs:
                # next line assumes parent's values already read in!
                for val in values[parent]: 
                    tmp.append(parentconfig+[(parent,val)])
            parentconfigs = tmp
        fobj.next() # skip 2 lines
        fobj.next()
        data = []
        for parentconfig in parentconfigs:
            line = fobj.next()
            for i, datum in enumerate(eval(tuple_re.search(line).group())):
                key = tuple(sorted(parentconfig+[(node,states[i])]))
                data.append((key,float(datum)))
        # sort data according to values of variables
        # to allow immediate construction of a factor
        data.sort()
        named_cpts[node] = (parents,[datum[1] for datum in data])

        if get_positions:
            match = coords_re.search(fobj.next())
            while not match:
                match = coords_re.search(fobj.next())
            coords[node] = match.groups()
            

def read_dnet2(fobj,get_positions=False):
    """Return CPTs of a Bayesian net in
    Netica 'dnet' format

    Does not work if inheritance is being used in the dnet file!
    @param fobj: File or name of file containing the input
    @type fobj: File or String
    @return: A dictionary. Each key is a node name each value is again
    a dictionary mapping fields to values. Fields typically include 'states',
    'probs', etc
    @rtype: Dictionary
    """
    from simpleparse.parser import Parser
    from simpleparse.dispatchprocessor import DispatchProcessor, dispatch
    
    dnet_bnf_grammar = r'''
    file :=         ws*, (bnet, ws*, ";")+, ws*
    bnet :=         "bnet", ws*, idname, ws*, "{", ws*, netstmt*, ws*, "}", ws*
    netstmt :=      (netequality/bnode/param/visnet/define), ws*, ";", ws*
    define :=       "define", whitespace, bnode
    netequality :=  netfield, ws*, "=", ws*, value, ws*
    bnode :=        "node", whitespace,  idname, ws*,  list?, "{", ws*,  nodestmt*, ws*,  "}", ws*
    list :=         "(", idname, ")", ws*     # only deal with single element lists
    nodestmt :=     (nodeequality/visnode), ws*, ";", ws*
    nodeequality := nodefield, ws*, "=", ws*, value, ws*, ws*
    param :=        "param", whitespace, idname, ws*, "{", ws*, paramstmt*, ws*, "}", ws*
    paramstmt :=    paramfield, ws*, "=", ws*, value, ws*, ";", ws*
    visnet :=       "visual", whitespace, idname, ws*, "{", ws*, vnetstmt*, ws*, "}", ws*
    vnetstmt :=     (vnetstmt1/vnetstmt2)
    vnetstmt1 :=     vnetfield, ws*, "=", ws*,  value, ws*, ";", ws*
    vnetstmt2 :=     -"{"+, "{", ws*, vnetstmt1*, ws*, "}", ws*, ";"  #workaround
    visnode :=      "visual", whitespace, idname, ws*, "{", ws*, vnodestmt*, ws*, "}", ws*
    vnodestmt :=    (vnodestmt1/vnodestmt2/vnodestmt3), ws*
    vnodestmt1 :=    vnodefield, ws*, "=", ws*, value, ";"
    vnodestmt2 :=    vislink, ws*, ";"
    vnodestmt3 :=   "font", -[\n]+       # work around, since this value has embedded semi-colons
    vislink :=      "link", whitespace, idname, ws*, "{", ws*, vlinkstmt*, ws*, "}", ws*
    vlinkstmt :=    vlinkfield, ws*,  "=", ws*,  value, ws*,  ";", ws*
    netfield :=     ("numdimensions"/"eqncontext"/"user"/
    "title"/"comment"/"author"/"whochanged"/"whenchanged"/
    "locked")
    nodefield :=    ("kind"/"discrete"/"measure"/"chance"/"numstates"/"states"/
                      "levels"/"units"/"inputs"/"parents"/"functable"/
                      "equation"/"probs"/"numcases"/"fading"/"delays"/
                      "persist"/"position"/"evidcost"/"user"/"title"/
                      "comment"/"author"/"whochanged"/"whenchanged"/"locked"/
                       "value"/"evidence"/"belief")
    paramfield :=  ("discrete"/"measure"/"numstates"/"states"/"levels"/"units")
    vnetfield :=   [A-Za-z0-9_]+   #since lots of extras now!
    vnodefield :=  ("center"/"size"/"dispform"/"hidden"/"height"/"links"/"user"/"parts")
    vlinkfield :=  ("path"/"labelposn"/"linewidth"/"hidden"/"shareseg"/"user")
    idname :=      [A-Za-z0-9_]+ # since 1 is apparently OK as an idname
    ws := (whitespace/comment1/comment2/comment3)
    comment1 := "//", -[\n]*, [\n]
    comment2 := "/*", -"*/"*, "*/"
    comment3 := "/", hash, -(hash, "/")*, hash, "/"
    whitespace :=         [ \t\n\r]+
    hash :=        "\x23"
    value :=       -";"+
    '''

    class MyProcessorClass( DispatchProcessor ):
        def __init__(self):
            self.nodes = {}
            self._id_is_class = False
        def _generic(self,(tag,start,stop,subtags),buffer,todo):
            for subtag in subtags:
                if subtag[0] in todo:
                    dispatch(self,subtag,buffer)
        def bnet(self,info,buffer):
            self._generic(info, buffer, ["netstmt"])
        def ws(self,info,buffer):
            pass
        def netstmt(self,info,buffer):
            self._generic(info,buffer,["bnode","define"])
        def bnode(self,info,buffer):
            self._generic(info,buffer,["idname","list", "nodestmt"])
        def define(self,info,buffer):
            self._generic(info,buffer,["bnode"])
        def list(self,info,buffer):
            self._id_is_class = True
            self._generic(info,buffer,["idname"])
            self._id_is_class = False
        def nodestmt(self,info,buffer):
            self._generic(info,buffer,["nodeequality","visnode"])
        def nodeequality(self,(tag,start,stop,subtags),buffer):
            for subtag in subtags:
                if subtag[0] == 'nodefield':
                    field = dispatch(self,subtag,buffer)
                elif subtag[0] == 'value':
                    value = dispatch(self,subtag,buffer)
            self.nodes[self._current_node][field] = value
        def visnode(self,info,buffer):
                pass

            # these following extract the actual text
            # ...
    
        def idname(self,(tag,start,stop,subtags),buffer):
            if self._id_is_class:  # have been sent the id of a class
                klass = buffer[start:stop]
                self.nodes[self._current_node].update(self.nodes[klass])
            else:                  # have been sent the id of a node
                current_node = buffer[start:stop] 
                self.nodes[current_node] = {}
                self._current_node = current_node
        def nodefield(self,(tag,start,stop,subtags),buffer):
            return buffer[start:stop]
        def value(self,(tag,start,stop,subtags),buffer):
            # have to delete comments etc
            return re.sub(r'(\s+|//.*)','',buffer[start:stop])

    my_processor = MyProcessorClass()
    Parser(dnet_bnf_grammar,'file').parse(fobj.read(),processor=my_processor)
    return my_processor.nodes


# def read_uci(id):
#    base_url = 'http://mlearn.ics.uci.edu/databases/'
#    return  urllib.urlopen('%s/%s/%s.data' % (base_url,id,id))

def read_twlib(i):
    """Fetch and parse a twlib graph from Utrecht

    @param i: Number of the graph
    @type i: Int
    @return: Vertices, Edges of the graph
    @rtype: Tuple
    """
    return read_dimacs(urllib.urlopen('http://people.cs.uu.nl/hansb/treewidthlib/graphfile.php?id=%d' % i))


def read_xdsl(fobj):
    raise Warning("Don't use! data not yet ordered correctly!")

    from xml.dom.minidom import parse
    values = {}
    named_cpts = {}
    for cpt in parse(fobj).getElementsByTagName('cpt'):
        var = cpt.getAttribute('id')
        values[var] = [state.getAttribute('id') for state in cpt.getElementsByTagName('state')]
        try:
            parents = cpt.getElementsByTagName('parents')[0].firstChild.data
        except IndexError:
            parents = []
        named_cpts[var] = (parents,
                           cpt.getElementsByTagName('probabilities')[0].firstChild.data)
    return values, named_cpts

class GraphCanvas(Tkinter.Canvas):
    """Class for drawing graphs

    @ivar _edit: Whether the graph may be edited by the user
    @type _edit: Boolean
    @ivar _colour_user_actions: Whether to colour vertices as a result of the
    user selecting them
    @type _colour_user_actions: Boolean
    @ivar _sel: Id of selected canvas object
    @type _sel: int
    @ivar _original_graph: The graph at the time the L{GraphCanvas}
    object is created
    @type _original_graph: L{Graph}
    @ivar _vertex_ids: Maps a vertex (string) to the id of the L{GraphCanvas} object displaying it
    @type _vertex_ids: Dictionary
    @ivar _pp_vertex: A function mapping graph vertices to the string used for
        the text of the C{Tkinter.Canvas} text object representing it.
    @type _pp_vertex: Function, returning a string
    """

    def __init__(self, graph, parent=None, edit=True, colour_user_actions=True,
                 pp_vertex=None,**config):
        """Initialises a GraphCanvas to be a TKinter Canvas with extras.

        C{graph} is not altered by any of this class's methods
        @param graph: Graph to be edited/displayed
        @type graph: L{Graph}
        @param parent: The canvas's parent widget 
        @type parent: Tkinter widget
        @param edit: Whether to allow the graph to be edited
        @type edit: Boolean
        @param colour_user_actions: Whether to colour user actions
        @type colour_user_actions: Boolean
        @param pp_vertex: A function mapping graph vertices to the string used for
        the text of the C{Tkinter.Canvas} text object representing it.
        If unsupplied the identity function is used
        @type pp_vertex: Function, returning a string
        @param config: Any extra configuration parameters
        @type config: various
        """
        
        Tkinter.Canvas.__init__(self,parent,**config)
        if pp_vertex is None:
            def pp_vertex(x): return x
        self._original_graph = graph.copy()
        self._colour_user_actions = colour_user_actions
        self._edit = edit
        self._pp_vertex = pp_vertex
        self.original_state()
        if edit:
            self.bind('<ButtonPress-1>', self._sel_or_new) # key bindings:
            self.bind('<ButtonPress-2>', self._drawarrow)   #  see documentation for these methods
            self.bind('<ButtonPress-3>', self._drawline)   #  see documentation for these methods
        else:
            self.bind('<ButtonPress-1>', self._sel_current) # key bindings:
        self.bind('<B1-Motion>',     self._movenode)   #  for more information

    def arc_config(self,vertex1,vertex2,**config):
        """Alter an arc/edge by named Tkinter options

        Does nothing silently if there is no arc between these vertices

        @param vertex1: A vertex in the graph at one end of the arc
        @type vertex1: Immutable
        @param vertex2: A vertex in the graph at the other end of the arc
        @type vertex2: Immutable
        @param config: Configuration options
        @type config: Dictionary of keywords
        @raise KeyError: If either vertex does not exist
        """
        t1 = self._id_to_tag(self._vertex_ids[vertex1])
        t2 = self._id_to_tag(self._vertex_ids[vertex2])
        for arc_id in self.find_withtag(t1):
            if t2 in self.gettags(arc_id):
                self.itemconfigure(arc_id,**config)


    def drawarc_vertices(self,vertex1,vertex2,**config):
        """Draw a line or arrow between two named vertices

        @param vertex1: First vertex
        @type vertex1: String
        @param vertex2: Second vertex
        @type vertex2: String
        @param config: Options to pass to C{create_line}
        @type config: Various
        @raise KeyError: If either C{vertex1} or C{vertex2} do not name
        an existing vertex
        """
        #@raise ArcError: If there is already a line between these vertices
        vertex_ids = self._vertex_ids
        self._drawarc2(vertex_ids[vertex1],
                       vertex_ids[vertex2],**config)


    def graph(self):
        """Return the displayed graph as a tuple

        Suitable for passing to L{Graphs.Graph.reinit}.
        Note that the vertices will be those displayed. These may have
        been altered from the vertices originally given at construction time using
        the C{pp_vertex} option of L{__init__}.
        
        @return: C{vertices, arrows, lines, vertex_positions} of the displayed graph
        @rtype: Tuple
        """
        vertices, arrows, lines, vertex_positions = [], [], [], {}
        ids = self.find_all()
        for ident in ids:
            if self.type(ident) == 'text':
                vertex = self.itemcget(ident,'text')
                vertices.append(vertex)
                vertex_positions[vertex] = tuple(self.coords(ident))
            elif self.type(ident) == 'line':
                if self.itemcget(ident,'arrow') == Tkinter.LAST:
                    arcs = arrows
                else:
                    arcs = lines
                id1, id2 = self._tags_to_ids(self.gettags(ident))
                arcs.append((self.itemcget(id1,'text'),self.itemcget(id2,'text')))
        return vertices, arrows, lines, vertex_positions

    def vertex_config(self,vertex,**config):
        """Alter a vertex by named Tkinter options

        @param vertex: A vertex in the graph
        @type vertex: String
        @param config: Configuration options
        @type config: Dictionary of keywords
        """
        self.itemconfigure(self._vertex_ids[vertex],**config)


    def original_state(self):
        """Revert Canvas to its state when originally created"""
        self.delete(Tkinter.ALL)
        graph = self._original_graph
        height, width = int(self['height']), int(self['width'])
        vertex_ids = {}
        vertex_positions = graph.vertex_positions()
        for node in graph.vertexlist():
            try:
                (x,y) = vertex_positions[node]
            except KeyError:
                (x,y) = random.randint(0,width), random.randint(0,height)
            vertex_ids[node] = self.create_text(x,y,text=self._pp_vertex(node))
        for (arcs,arrow_type) in ((graph.arrows(),Tkinter.LAST),(graph.lines(),None)):
            for node1, node2 in arcs:
                node_id1, node_id2 = vertex_ids[node1], vertex_ids[node2] 
                self._drawarc2(node_id1,node_id2,arrow=arrow_type)
        self._sel = None
        self._vertex_ids = vertex_ids

        
    @staticmethod
    def _bbsect(bb,origin):
        """Gets intersection of bounding box and a line.

        Given a line drawn between the centre of a rectangle
        and some other point, finds the intersection of that line
        and the rectangle's boundary.
        This is used to prevent arcs overwriting the text which
        represents a node.
        @param bb: Bounding box
        @type bb: Tuple
        @param origin: Origin of line
        @type origin: Tuple
        """
        (x1,y1,x2,y2) = bb
        (x,y) = origin
        x_m, y_m = (x1+x2)/2.0, (y1+y2)/2.0            # find rectangle centre
        if x < x_m:                                    # perform reflections: 
            x1,x2 = x2,x1                              #  so that afterwards we can
        if y < y_m:                                  #  pretend (almost) that intersection is in 
            y1,y2 = y2,y1                              #  top right quadrant of rectangle
        try:
            g = (y-y_m)/(x-x_m)                        # get gradient of line
        except ZeroDivisionError:                      # if gradient infinite:
            return x_m,y2                              #  intersection due north of centre
        if abs(g) > abs((y2-y1+0.0)/(x2-x1)):          # compare g to gradient of diagonal:
            return x_m+(y2-y_m)/g, y2                  #  (abs needed since we may have reflected)
        else:                                          #  if g greater then intersection at top
            return x2, y_m+g*(x2-x_m)                  #  else it's at the side


    def _drawarc(self,**config):
        """Draws a line or arrow from current node under pointer to selected node.

        Does various tedious checks and then draws arc.
        The arc is tagged with the tags of the nodes it connects.
        This is the only place parent-child relations are stored.

        @param config: Options to pass to L{_drawarc2}
        @param config: Various
        """
        try:
            this = self.find_withtag(Tkinter.CURRENT)[0]     # 'this' is id of item under pointer
        except IndexError:                                   # if no such item:
            return                                           #  do nothing silently
        if this == self._sel or self.type(this) != 'text': # if current not a node or is selected:
            return                                           #  do nothing silently
        self._drawarc2(this,self._sel,**config)
        
    def _drawarc2(self,node_id1,node_id2,**config):
        """Draw a line or arrow between two vertices

        @param node_id1: Id of first vertex
        @type node_id1: int
        @param node_id2: Id of second vertex
        @type node_id2: int
        @param config: Options to pass to C{create_line}
        @type config: Various
        """
        #@raise ArcError: If there is already a line between these vertices
        t1,t2 = self._id_to_tag(node_id1),self._id_to_tag(node_id2)
#         for arc in self.find_withtag(t1):
#             if t2 in self.gettags(arc):
#                 raise ArcError("Already an arc between items %d and %d" %
#                                (node_id1, node_id2))
        (x1,y1),(x2,y2) = self._ends((node_id1,node_id2))
        self.create_line(x1,y1,x2,y2,tags=(t1,t2),**config)

    def _drawarrow(self,event):
        """Draw an arrow from current node under pointer to selected node.

        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        self._drawarc(arrow=Tkinter.LAST)

    def _drawline(self,event):
        """Draw a line from current node under pointer to selected node.

        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        self._drawarc()

    def _ends(self,ends):
        """Given two nodes, finds co-ords for an arc between them.

        Arc will not overwrite the node text.
        @param ends: The two node ids
        @type ends: Tuple of ints
        @return: The co-ords at the C{frm} end and then the co-ords for the C{to} end
        @rtype: Tuple
        """
        frm, to = ends
        return (self._bbsect(self.bbox(frm),self.coords(to  )),  # co-ords at 'from' end
                self._bbsect(self.bbox(to ),self.coords(frm )))  # co-ords at 'to' end

    @staticmethod
    def _id_to_tag(id):
        """Returns a node's unique tag from its Canvas id.

        @param id: Id of a node
        @type id: int
        @return: The unique tag for a node
        @rtype: String
        """
        return 't%d' % id


    def _movenode(self, event):
        """Moves the selected node to a new position on the Canvas.

        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        if self._sel is None or self.type(self._sel) != 'text': # if a non-node is selected:
            return                                         #  do nothing silently
        self.coords(self._sel,event.x,event.y)              # move the node
        for arc in self.find_withtag(self._id_to_tag(self._sel)): # find all its arcs
            (x1,y1),(x2,y2) = self._ends(self._tags_to_ids(self.gettags(arc))) #  get new co-ords
            self.coords(arc,x1,y1,x2,y2)                                #  move arc to new co-ords


    def _newnode(self,event):
        """Creates a node

        Note that the node name given by the user is passed through
        the instance's C{_pp_vertex} function.
        
        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        win=Tkinter.Frame(self)
        ent = Tkinter.Entry(win)
        ent.pack()
        var = Tkinter.StringVar()
        ent.config(textvariable=var)
        ent.focus_set()
        button = Tkinter.Button(win,text='OK',command=win.destroy)
        button.bind('<Return>', lambda event: win.destroy())
        button.pack()
        cwin = self.create_window(event.x,event.y,window=win)
        win.wait_window()
        name = var.get()
        self.delete(cwin)
        if name:
            ident = self.create_text(event.x,event.y,text=self._pp_vertex(name))  # produce Canvas object
            self._select(ident)
            self._vertex_ids[name] = ident
        else:
            print 'Ignoring unnamed node'


    def _select(self,ident):
        """Selects a Canvas object using its id.

        @param ident: Id of object to be selected
        @type ident: int
        """
        if self._colour_user_actions and self._sel:    # if something already selected:
            self.itemconfig(self._sel,fill='black')  #  make it black
        self._sel = ident                               # update sel attribute of GMCanvas object
        if self._colour_user_actions:
            self.itemconfig(ident,fill='red')        # make selected object red

    def _sel_current(self,event):
        """Selects a node.

        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        try:
            self._select(self.find_withtag(Tkinter.CURRENT)[0])   # is pointer on an object?:
        except IndexError:                               #  if pointer NOT on an object:
            pass

    def _sel_or_new(self,event):
        """Selects or creates a node.

        @param event: Event at time this is called
        @type event: C{Tkinter.Event}
        """
        try:
            self._select(self.find_withtag(Tkinter.CURRENT)[0])   # is pointer on an object?:
        except IndexError:                               #  if pointer NOT on an object:
            if self._edit:
                self._newnode(event)                              #    create a new node

#     def str_to_ids(self,str1,str2):
#         dkt = {}
#         for ident in self.find_all():
#             ident_str = self.itemcget(ident,'text')
#             if ident_str == str1 or ident_str == str2:
#                 dkt[ident_str] = ident
#                 if len(dkt) == 2:
#                     return dkt[str1], dkt[str2]
#         raise ValueError("%s or %s (or both) missing" % (str1,str2))

    @staticmethod
    def _tags_to_ids(tags):
        """Returns two node ids given corresponding two node tags.

        @param tags: Pairs of tags
        @type tags: Tuple
        @return: Ids corresponding to the tags
        @rtype: Tuple
        """
        tag1,tag2 = tags
        return int(tag1[1:]), int(tag2[1:])

    def zap(self):
        """Delete the selected node or arc."""
        if self.type(self._sel) == 'text':                  # if a node:
            self.delete(self._id_to_tag(self._sel))               #  deletes any connected arcs
        self.delete(self._sel)                              # deletes selected from Canvas
    
