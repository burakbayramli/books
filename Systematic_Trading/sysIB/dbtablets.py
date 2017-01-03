"""
Time series database example


"""

from sysIB.dbconnect import connection
import pandas as pd
import datetime


def date_as_string(dtobject=datetime.datetime.now()):
    return str(dtobject.strftime("%Y-%m-%d %H:%M:%S.%f"))


class tsdata(object):
    '''
    Very basic table for ts data
    
    Does not do checking eg for multiple records etc    
    '''


    def __init__(self, dbname):
        
        self.connection=connection(dbname)
    
    def add(self, code, pandasts):
        for datetime in pandasts.index:
            price=pandasts[datetime]
            self.addrow(code, datetime, price)
    
    def addrow(self, code, datetime, price):
        self.connection.write("INSERT INTO timeseries VALUES (?, ?, ?)", (date_as_string(datetime), code, float(price)))
    
    def read(self, code):
        ans=self.connection.read("SELECT datetime, price FROM timeseries WHERE code=?", (code,))
        dindex=[pd.to_datetime(x[0]) for x in ans]
        prices=[float(x[1]) for x in ans]
        ans=pd.TimeSeries(prices, index=dindex)
        return ans
    

