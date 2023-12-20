#
# Copyright (c) 2009-2020 fem2ufo
#

# Python stdlib imports
#from contextlib import closing
#import zipfile
import sqlite3 as sqlite3
#from sqlite3 import Error
#from datetime import datetime
#


#
def create_connection(db_file):
    """ create a database connection to a SQLite database
    """
    try:
        conn = sqlite3.connect(db_file)
        #print('sqlite version {:} {:}'.format(sqlite3.version, sqlite3.sqlite_version_info))
        return conn
    except sqlite3.Error as e:
        raise RuntimeError(e)
    #finally:
    #    conn.close()
    #return None
#
def create_table(conn, create_table_sql):
    """ create a table from the create_table_sql statement
    :param conn: Connection object
    :param create_table_sql: a CREATE TABLE statement
    :return:
    """
    #with closing(conn.cursor()) as cursor:
    #    cursor.execute(create_table_sql)
    #
    try:
        c = conn.cursor()
        c.execute(create_table_sql)
    except sqlite3.Error as e:
        raise RuntimeError(e)
#
#
#
# ------------------------------------------------------------------
# SQL
#
def get_load_data(conn, load_name:int|str, load_type: str):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Load \
                 WHERE name = {:} AND type = '{:}'".format(load_name, load_type))
    loads = cur.fetchone()
    return loads
#
#
def check_nodes(conn, node_name: int|str):
    """check if node exist"""
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Nodes \
                 WHERE name = {:} ".format(node_name))
    node = cur.fetchone()
    return node    
#
#
def check_element(conn, element_name):
    """ """
    cur = conn.cursor()
    cur.execute ("SELECT * FROM tb_Elements\
                WHERE tb_Elements.name = {:};".format(element_name))
    row = cur.fetchone()
    return row