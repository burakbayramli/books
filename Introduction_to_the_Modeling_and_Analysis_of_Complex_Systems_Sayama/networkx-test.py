import networkx as nx

# creating a new empty Graph object
g = nx.Graph()

# adding a node named 'John'
g.add_node('John')

# adding a bunch of nodes at once
g.add_nodes_from(['Josh', 'Jane', 'Jess', 'Jack'])

# adding an edge between 'John' and 'Jane'
g.add_edge('John', 'Jane')

# adding a bunch of edges at once
g.add_edges_from([('Jess', 'Josh'), ('John', 'Jack'), ('Jack', 'Jane')])

# adding more edges
# undefined nodes will be created automatically
g.add_edges_from([('Jess', 'Jill'), ('Jill', 'Jeff'), ('Jeff', 'Jane')])

# removing the edge between 'John' and 'Jane'
g.remove_edge('John', 'Jane')

# removing the node 'John'
# all edges connected to that node will be removed too
g.remove_node('John')
