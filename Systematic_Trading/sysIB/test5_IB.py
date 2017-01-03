from sysIB.wrapper_v5 import IBWrapper, IBclient
from swigibpy import Contract as IBcontract

import time
 
if __name__=="__main__":

    """
    This simple example places an order, checks to see if it is active, and receives fill(s)
    
    Note: If you are running this on the 'edemo' account it will probably give you back garbage
    
    Though the mechanics should still work
    
    This is because you see the orders that everyone demoing the account is trading!!!
    """

    callback = IBWrapper()
    client=IBclient(callback)
    
    ibcontract = IBcontract()
    ibcontract.secType = "FUT"
    ibcontract.expiry="201509"
    ibcontract.symbol="GBL"
    ibcontract.exchange="DTB"

    ## Get contract details
    cdetails=client.get_contract_details(ibcontract)
    
    ## In particular we want the expiry. You cannot just use cdetails['expiry'][:6] to map back to the yyyymm 
    ##    expiry since certain contracts expire the month before they should!
    
    print "Expiry is %s" % cdetails['expiry']

    ## Once you have the portfolio positions then you can use these expiries to map back

    (account_value, portfolio_data)=client.get_IB_account_data()

    print "Portfolio"
    print portfolio_data
    
    print "\n account info"
    print account_value
