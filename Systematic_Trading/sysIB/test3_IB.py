from sysIB.wrapper_v3 import IBWrapper, IBclient
from swigibpy import Contract as IBcontract
 
if __name__=="__main__":

    """
    This simple example returns streaming price data
    """

    callback = IBWrapper()
    client=IBclient(callback)
    
    ibcontract = IBcontract()
    ibcontract.secType = "FUT"
    ibcontract.expiry="201612"
    ibcontract.symbol="GE"
    ibcontract.exchange="GLOBEX"

    ans=client.get_IB_market_data(ibcontract)
    print "Bid size, Ask size; Bid price; Ask price"
    print ans
    