from sysIB.wrapper import IBWrapper, IBclient

 
if __name__=="__main__":

    """
    This simple example returns the time 
    """

    callback = IBWrapper()
    client=IBclient(callback)
    print client.speaking_clock() 
