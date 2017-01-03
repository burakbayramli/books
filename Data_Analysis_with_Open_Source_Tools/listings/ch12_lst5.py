
from SimPy.Simulation import *
import random as rnd

interarrival_time = 10.0
service_time = 8.0


class CustomerGenerator( Process ):
    def produce( self, b ):
        while True:
            c = Customer( b )
            c.start( c.doit() )
            yield hold, self, rnd.expovariate(1.0/interarrival_time)


class Customer( Process ):
    def __init__( self, resource ):
        Process.__init__( self )
        self.bank = resource
    
    def doit( self ):
        yield request, self, self.bank
        yield hold, self, self.bank.servicetime()
        yield release, self, self.bank


class Bank( Resource ):
    def servicetime( self ):
        return rnd.expovariate(1.0/service_time)


initialize()

bank = Bank( capacity=1, monitored=True, monitorType=Monitor )

src = CustomerGenerator()
activate( src, src.produce( bank ) )

simulate( until=500 )

print bank.waitMon.mean()
print

for evt in bank.waitMon:
    print evt[0], evt[1]
