from sysIB.wrapper_v4 import IBWrapper, IBclient
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

    ## Place the order, asking IB to tell us what the next order id is
    ## Note limit price of zero
    orderid1=client.place_new_IB_order(ibcontract, 10, 0.0, "MKT", orderid=None)
    
    print ""
    print "Placed market order, orderid is %d" % orderid1
    
    ## And here is a limit order, unlikely ever to be filled
    ## Note limit price of 100
    orderid2=client.place_new_IB_order(ibcontract, -10, 200.0, "LMT", orderid=None)
    print ""
    print "Placed limit order, orderid is %d" % orderid2

    
    
    ## Short wait so dialog is in order, not neccessary
    time.sleep(5)
    
        
    order_structure=client.get_open_orders()
    
    ## Note that the market order has probably filled by now
    
    print "Active orders: (should just be limit order)"
    print order_structure
    print ""
    
    print "Cancelling remaining orders"
    for order_ref in order_structure.keys():
        order=order_structure[order_ref]
        print "Cancelling %d" % order['orderid']   
        client.tws.cancelOrder(order['orderid'])

    print "Waiting for cancellation to finish"    
    while client.any_open_orders():
        pass
    print "No active orders now"
    print client.any_open_orders()
    print ""

    
    ## Get the executions (gives you everything for last business day)
    execlist=client.get_executions()
    
    print "Following executed since midnight this morning:"
    print ""
    print execlist