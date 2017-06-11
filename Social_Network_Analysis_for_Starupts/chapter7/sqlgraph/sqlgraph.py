from functools import wraps

__author__ = "Alex Kouznetsov"

import networkx
import sqlite3
import simplejson
import base64

def _prepare_tables(conn, name):
    c = conn.cursor()
    c.execute('''drop table if exists "%s_edges"''' % name)
    c.execute('''drop table if exists "%s_nodes"''' % name)
    c.execute('''create table "%s_nodes" (node, attributes)''' % name)
    c.execute('''create table "%s_edges" (efrom, eto, attributes)''' % name)
    conn.commit()
    c.close()

def write_sqlite(G, sqlfile):
    ''' Graphs with same names will overwrite each other'''
    sq = SqlGraph(sqlfile, G.name)
    sq.from_nx(G)
    return sq

def read_sqlite(sqlfile, name):
    sq = SqlGraph(sqlfile, name)
    return sq.to_nx()

def _encode(name):
    ''' Before using graph names in tables we need to convert
    to something that won't upset sqlite.'''
    return base64.encodestring(name).replace('\n', '')

def _decode(name):
    return base64.decodestring(name)

def cursored(func):
    @wraps(func)
    def wrapper(sqlgraph, *args, **kwargs):
        supplied_cursor = kwargs.get('cursor', None)
        cursor = supplied_cursor or sqlgraph.conn.cursor()
        kwargs['cursor'] = cursor
        result = func(sqlgraph, *args, **kwargs)
        if not supplied_cursor:
            cursor.connection.commit()
            cursor.close()
        return result
    return wrapper


class SqlGraph(object):
    def __init__(self, sqlfile, name):
        self.conn = sqlite3.connect(sqlfile)
        self.name = _encode(name)

    @cursored
    def add_node(self, node, attr_dict=None, cursor=None):
        attributes = simplejson.dumps(attr_dict)
        cursor.execute('''insert or replace into "%s_nodes" (node, attributes)
            values(?,?)''' % self.name, (node, attributes))

    @cursored
    def add_edge(self, fromnode, tonode, attr_dict=None, cursor=None):
        attributes = simplejson.dumps(attr_dict)
        cursor.execute('''insert or replace into "%s_edges" (efrom, eto, attributes)
            values(?,?,?)''' % self.name, (fromnode, tonode, attributes))

    @cursored
    def remove_node(self, node, cursor=None):
        cursor.execute('delete from "%s_nodes" where node=?' % self.name, (node,))

    @cursored
    def remove_edge(self, fromnode, tonode, cursor=None):
        cursor.execute('delete from "%s_edges" where efrom=? and eto=?' % self.name, (fromnode, tonode))

    @cursored
    def get_node_data(self, node, cursor=None):
        result = cursor.execute('select * from "%s_nodes" where node=?'%self.name, (node,))
        for row in result:
            return row[0]
        else:
            raise Exception('Node %s is not in graph.' % node)

    @cursored
    def get_edge_data(self, fromnode, tonode, cursor=None):
        result = cursor.execute('select * from "%s_edges" where efrom=? and eto=?' % self.name, (fromnode, tonode))
        for row in result:
            return row[0]
        else:
            raise Exception('Edge %s:%s is not in graph.' % (fromnode, tonode))

    def from_nx(self, G):
        self.name = _encode(G.name)
        _prepare_tables(self.conn, self.name)
        c = self.conn.cursor()
        for node, attr_dict in G.node.items():
            self.add_node(node, attr_dict, cursor=c)
        for efrom, eto, attr_dict in G.edges(data=True):
            self.add_edge(efrom, eto, attr_dict, cursor=c)
        self.conn.commit()
        c.close()

    def to_nx(self):
        G = networkx.Graph()
        c = self.conn.cursor()
        for row in c.execute('select * from "%s_nodes"' % self.name):
            G.add_node(row[0], attr_dict=simplejson.loads(row[1]))
        for row in c.execute('select * from "%s_edges"' % self.name):
            G.add_edge(row[0], row[1], attr_dict=simplejson.loads(row[2]))
        self.conn.commit()
        c.close()
        G.name = _decode(self.name)
        return G
