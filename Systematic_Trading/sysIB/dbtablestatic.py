"""

Static example
"""

from sysIB.dbconnect import connection

class staticdata(object):
    '''
    Very basic table for static data
    
    Does not do checking eg for multiple records etc    
    '''


    def __init__(self, dbname):
        
        self.connection=connection(dbname)
    
    def add(self, code, fullname):
        self.connection.write("INSERT INTO static VALUES (?, ?)", (code, fullname))
    
    def modify(self, code, newname):
        self.connection.write("UPDATE static SET fullname=? WHERE code=?", (newname, code))

    def read(self, code):
        ans=self.connection.read("SELECT fullname FROM static WHERE code=?", (code,))
        if len(ans)==0:
            return None
        return str(ans[0][0])
    
    def delete(self, code):
        self.connection.write("DELETE FROM static WHERE code=?", (code,))
        