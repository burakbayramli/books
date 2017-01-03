from swigibpy import EWrapper
import time
import numpy as np
import datetime
from swigibpy import EPosixClientSocket

MEANINGLESS_ID=502

def return_IB_connection_info():
    """
    Returns the tuple host, port, clientID required by eConnect
   
    """
   
    host=""
   
    port=4001
    clientid=999
   
    return (host, port, clientid)

class IBWrapper(EWrapper):
    """

    Callback object passed to TWS, these functions will be called directly by the TWS or Gateway.

    """

    ## We need these but don't use them
    def nextValidId(self, orderId):
        pass
   
    def managedAccounts(self, openOrderEnd):
        pass

    ## error handling

    def init_error(self):
        setattr(self, "flag_iserror", False)
        setattr(self, "error_msg", "")

    def error(self, id, errorCode, errorString):
        """
        error handling, simple for now
       
        Here are some typical IB errors
        INFO: 2107, 2106
        WARNING 326 - can't connect as already connected
        CRITICAL: 502, 504 can't connect to TWS.
            200 no security definition found
            162 no trades

        """
        ## Any errors not on this list we just treat as information
        ERRORS_TO_TRIGGER=[201, 103, 502, 504, 509, 200, 162, 420, 2105, 1100, 478, 201, 399]
       
        if errorCode in ERRORS_TO_TRIGGER:
            errormsg="IB error id %d errorcode %d string %s" %(id, errorCode, errorString)
            print errormsg
            setattr(self, "flag_iserror", True)
            setattr(self, "error_msg", True)
           
        ## Wrapper functions don't have to return anything


        

    def init_tickdata(self, TickerId):
        if "data_tickdata" not in dir(self):
            tickdict=dict()
        else:
            tickdict=self.data_tickdata

        tickdict[TickerId]=[np.nan]*4
        setattr(self, "data_tickdata", tickdict)


    def tickString(self, TickerId, field, value):
        marketdata=self.data_tickdata[TickerId]

        ## update string ticks

        tickType=field

        if int(tickType)==0:
            ## bid size
            marketdata[0]=int(value)
        elif int(tickType)==3:
            ## ask size
            marketdata[1]=int(value)

        elif int(tickType)==1:
            ## bid
            marketdata[0][2]=float(value)
        elif int(tickType)==2:
            ## ask
            marketdata[0][3]=float(value)
        


    def tickGeneric(self, TickerId, tickType, value):
        marketdata=self.data_tickdata[TickerId]

        ## update generic ticks

        if int(tickType)==0:
            ## bid size
            marketdata[0]=int(value)
        elif int(tickType)==3:
            ## ask size
            marketdata[1]=int(value)

        elif int(tickType)==1:
            ## bid
            marketdata[2]=float(value)
        elif int(tickType)==2:
            ## ask
            marketdata[3]=float(value)
        
        
        
           
    def tickSize(self, TickerId, tickType, size):
        
        ## update ticks of the form new size
        
        marketdata=self.data_tickdata[TickerId]

        
        if int(tickType)==0:
            ## bid
            marketdata[0]=int(size)
        elif int(tickType)==3:
            ## ask
            marketdata[1]=int(size)
        

   
    def tickPrice(self, TickerId, tickType, price, canAutoExecute):
        ## update ticks of the form new price
        
        marketdata=self.data_tickdata[TickerId]
        
        if int(tickType)==1:
            ## bid
            marketdata[2]=float(price)
        elif int(tickType)==2:
            ## ask
            marketdata[3]=float(price)
        
    
    def updateMktDepth(self, id, position, operation, side, price, size):
        """
        Only here for completeness - not required. Market depth is only available if you subscribe to L2 data.
        Since I don't I haven't managed to test this.
        
        Here is the client side call for interest
        
        tws.reqMktDepth(999, ibcontract, 9)
        
        """
        pass

        
    def tickSnapshotEnd(self, tickerId):
        
        print "No longer want to get %d" % tickerId





class IBclient(object):
    """
    Client object
    
    Used to interface with TWS for outside world, does all handling of streaming waiting etc
    
    Create like this
    callback = IBWrapper()
    client=IBclient(callback)

    We then use various methods to get prices etc

    """
    def __init__(self, callback):
        """
        Create like this
        callback = IBWrapper()
        client=IBclient(callback)
        """
        
        tws = EPosixClientSocket(callback)
        (host, port, clientid)=return_IB_connection_info()
        tws.eConnect(host, port, clientid)

        self.tws=tws
        self.cb=callback


        
    def get_IB_market_data(self, ibcontract, seconds=30, tickerid=MEANINGLESS_ID):
        """
        Returns granular market data
        
        Returns a tuple (bid price, bid size, ask price, ask size)
        
        """
        
        
        ## initialise the tuple
        self.cb.init_tickdata(tickerid)
        self.cb.init_error()
            
        # Request a market data stream 
        self.tws.reqMktData(
                tickerid,
                ibcontract,
                "",
                False)       
        
        start_time=time.time()

        finished=False
        iserror=False

        
        while not finished and not iserror:
            iserror=self.cb.flag_iserror
            if (time.time() - start_time) > seconds:
                finished=True
            pass
        self.tws.cancelMktData(tickerid)
        
        marketdata=self.cb.data_tickdata[tickerid]
        ## marketdata should now contain some interesting information
        ## Note in this implementation we overwrite the contents with each tick; we could keep them
        
        
        if iserror:
            print "Error: "+self.cb.error_msg
            print "Failed to get any prices with marketdata"
        
        return marketdata
    
    
    
