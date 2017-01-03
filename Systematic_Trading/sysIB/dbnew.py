

import sqlite3
import os
import stat




def setup_blank_tables(dbfilename, setupcodelist):
    
    """
    setup a new sqllite database
    
    """
    
    with sqlite3.connect(dbfilename) as conn:
        for setuptable in setupcodelist:
            conn.execute(setuptable)
            conn.commit()
    
    
    ## Set permissions
    os.chmod(dbfilename, stat.S_IRWXU | stat.S_IRGRP | stat.S_IROTH)

    print "created %s " % dbfilename
