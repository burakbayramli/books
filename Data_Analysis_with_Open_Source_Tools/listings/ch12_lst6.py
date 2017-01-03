
from SimPy.Simulation import *
import random as rnd

interarrival_time = 10.0

class CustomerGenerator( Process ):
    def produce( self, bank ):
        while True:
            c = Customer( bank, sim=self.sim )
            c.start( c.doit() )
            yield hold, self, rnd.expovariate(1.0/interarrival_time)


class Customer( Process ):
    def __init__( self, resource, sim=None ):
        Process.__init__( self, sim=sim )
        self.bank = resource
    
    def doit( self ):
        yield request, self, self.bank
        yield hold, self, self.bank.servicetime()
        yield release, self, self.bank


class Bank( Resource ):
    def setServicetime( self, s ):
        self.service_time = s
        
    def servicetime( self ):
        return rnd.expovariate(1.0/self.service_time )


def run_simulation( t, steps, runs ):
    for r in range( runs ):
        sim = Simulation()
        sim.initialize()
        
        bank = Bank( monitored=True, monitorType=Tally, sim=sim )
        bank.setServicetime( t )

        src = CustomerGenerator( sim=sim )
        sim.activate( src, src.produce( bank ) )

        sim.startCollection( when=steps//2 )
        sim.simulate( until=steps )
        
        print t, bank.waitMon.mean()
    

t = 0
while t <= 11.0:
    t += 0.5
    run_simulation( t, 100000, 10 )
