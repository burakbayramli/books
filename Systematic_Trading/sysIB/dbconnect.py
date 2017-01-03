"""

This abstracts the database connection, i.e. exact file and way of accessing

The init of the object returns a database connection object which can be used to do common things


"""

import sqlite3


DBDICT=dict(mydb="mydb.db")

def get_db_filename(dbname):
    
    try:
        dbfilename=DBDICT[dbname]
    except KeyError:
        raise Exception("%s not in DBDICT" % dbname)
    
    return dbfilename

def get_db_connsql3_for(dbname):

    """
    
    Database connections
    
    Returns a sqllite3 connection 

    
    """

    dbfilename=get_db_filename(dbname)

    try:
        conn=sqlite3.connect(dbfilename, timeout=30)
    except:
        error_msg="Couldn't connect to database specified as %s resolved to %s" % (dbfilename, dbfilename)
        
        raise Exception(error_msg)

    return conn


class connection(object):
    '''
    object is a connection    '''


    def __init__(self, dbname):
        
        self.conn=get_db_connsql3_for(dbname)
    
    def close(self):
        """
        Close the database connection
        """
        self.conn.close()

    def write(self, sqltext, argtuplelist=[]):
        """
        
        """
        
        if len(argtuplelist)==0:
            self.conn.execute(sqltext)
        else:
            self.conn.execute(sqltext, argtuplelist)
        self.conn.commit()
    
    def read(self, sqltext, argtuplelist=[]):
        """
        Perform a generic select command, returns list of lists
        """
        
        self.conn.row_factory=sqlite3.Row
        if len(argtuplelist)==0:
            ans=self.conn.execute(sqltext)
        else:
            ans=self.conn.execute(sqltext, argtuplelist)
        ans=ans.fetchall()

        return ans
    