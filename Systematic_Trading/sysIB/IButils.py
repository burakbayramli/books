import pandas as pd
import numpy as np

DEFAULT_VALUE=np.nan

class autodf(object):
    '''
    Object to make it easy to add data in rows and return pandas time series
    
    Initialise with autodf("name1", "name2", ...)
    Add rows with autodf.add_row(name1=..., name2=...., )
    To data frame with autodf.to_pandas
    '''

    def __init__(self, *args):
        
        
        storage=dict()
        self.keynames=args
        for keyname in self.keynames:
            storage[keyname]=[]
            
        self.storage=storage 
        
    def add_row(self, **kwargs):
        
        for keyname in self.storage.keys():
            if keyname in kwargs:
                self.storage[keyname].append(kwargs[keyname])
            else:
                self.storage[keyname].append(DEFAULT_VALUE)

    def to_pandas(self, indexname=None):
        if indexname is not None:
            data=self.storage
            index=self.storage[indexname]
            data.pop(indexname)
            return pd.DataFrame(data, index=index)
        else:
            return pd.DataFrame(self.storage)
        
def bs_resolve(x):
    if x<0:
        return 'SELL'
    if x>0:
        return 'BUY'
    if x==0:
        raise Exception("trying to trade with zero")
        
def action_ib_fill(execlist):
    """
    Get fills (eithier ones that have just happened, or when asking for orders)
    
    Note that fills are cumulative, eg for an order of +10 first fill would be +3, then +9, then +10
    implying we got 3,6,1 lots in consecutive fills
    
    The price of each fill then is the average price for the order so far 
    """
    
    print "recived fill as follows:"
    print ""
    print execlist
    print ""
        
