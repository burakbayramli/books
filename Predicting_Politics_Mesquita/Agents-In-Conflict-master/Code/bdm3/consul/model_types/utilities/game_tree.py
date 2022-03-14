'''
Extensive-Form Game Tree

This module defines an extensive-form game tree. A tree is composed of nodes,
linked to each other. Connections between nodes are labeled as 'moves': a
player at a given node has several moves, each of which result in a different
node. This continues until the terminal nodes.

It does not store any 'current node' value, or include which player is playing
at each node, or payoffs; these will be defined in the Game object, which uses
the tree.

#TODO: Implement information sets.
'''
import json

class TreeNode(object):
    '''
    A node in a game tree.

    Attributes:
        name: The node's unique name.
        final: Boolean; is this a terminal node?
        children: Dictionary mapping moves to the name of the node they lead to.
    '''

    name = None
    final = False
    children = None

    def __init__(self, name, children=None):
        '''
        Create a new tree node.

        Args:
            name:
            children:
        '''

        self.name = name
        if children is None:
            self.final = True
        else:
            self.children = children

    def __getitem__(self, index):
        '''
        Get the next node associated with the given move.
        '''
        if index in self.children:
            return self.children[index]
        else:
            raise Exception("No such move.")


class Tree(object):
    '''
    Extensive-form game tree.

    Attributes:
        nodes: Dictionary of node names to objects.

    '''

    nodes = {}
    root_node = None

    def __init__(self):
        self.nodes = {}
        self.root_node = None

    def add_node(self, node, ignore_missing_children=False):
        '''
        Insert a new Node into the game tree.
        '''

        if node.name in self.nodes:
            raise Exception("A node of that name is already in the game tree")
        if not node.final and not ignore_missing_children:
            for child in node.children.values():
                if child not in self.nodes:
                    raise Exception("Child node not in tree.")

        self.nodes[node.name] = node

    def __getitem__(self, index):
        '''
        Overload the [square bracket] access.
        '''
        if index in self.nodes:
            return self.nodes[index]
        else:
            raise Exception("Node not found.")

    def get_final_nodes(self):
        '''
        Return a list of terminal nodes.
        '''
        end_nodes = []
        for name, node in self.nodes.items():
            if node.final:
                end_nodes.append(name)
        return end_nodes

    def print_tree(self, start_node, depth=0, show=False):
        '''
        Print the tree, or a subset of it.
        '''
        node = self[start_node]
        string = "  "*depth + node.name + "\n"
        if node.final:
            return string
        else:
            for next_node in node.children.values():
                string += self.print_tree(next_node, depth+1, False)
        if depth == 0 and show:
            print(string)
        return string

    def __str__(self):
        '''
        How to print the tree.
        '''
        if self.root_node is not None:
            return self.print_tree(self.root_node)
        else:
            return "No root node defined."

    # Code for writing to and reading from file.
    def to_file(self, filepath):
        '''
        Convert the tree to JSON.
        '''
        out_nodes = {node.name: node.children for node in self.nodes.values()}
        out = {"root_node": self.root_node, "tree": out_nodes}
        with open(filepath, "w") as f:
            json.dump(out, f) 

    @staticmethod
    def load_tree(filepath):
        '''
        Create a new Tree object from JSON.
        '''
        with open(filepath) as f:
            in_dict = json.load(f)
        new_tree = Tree()
        tree_dict = in_dict["tree"]
        for node, children in tree_dict.items():
            node = TreeNode(node, children)
            new_tree.add_node(node, ignore_missing_children=True)
        new_tree.root_node = in_dict["root_node"]
        return new_tree
