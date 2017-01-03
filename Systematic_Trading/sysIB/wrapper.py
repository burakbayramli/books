from swigibpy import EWrapper
import time
from swigibpy import EPosixClientSocket

### how many seconds before we give up
MAX_WAIT=30

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

        Callback object passed to TWS, these functions will be called directly
    by TWS.

    """

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


    def currentTime(self, time_from_server):

        setattr(self, "data_the_time_now_is", time_from_server)

    ### stuff we don't use
    def nextValidId(self, orderId):
        pass

    def managedAccounts(self, openOrderEnd):
        pass

class IBclient(object):
    def __init__(self, callback):
        tws = EPosixClientSocket(callback)
        (host, port, clientid)=return_IB_connection_info()
        tws.eConnect(host, port, clientid)

        self.tws=tws
        self.cb=callback

    def speaking_clock(self):
        print "Getting the time... "

        self.tws.reqCurrentTime()

        start_time=time.time()

        self.cb.init_error()

        iserror=False

        while not iserror:
            isfinished = hasattr(self.cb, 'data_the_time_now_is')
            if isfinished:
                break

            iserror=self.cb.flag_iserror

            if (time.time() - start_time) > MAX_WAIT:
                not_finished=False

            if iserror:
                not_finished=False

        if iserror:
            print "Error happened"
            print self.cb.error_msg

        return self.cb.data_the_time_now_is

