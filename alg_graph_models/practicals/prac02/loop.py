"""I don't know what the best way to do this is, but this at least works
and is simple
"""
from gPy.Graphs import UGraph

class MyUGraph(UGraph):

    def in_loop(self,vertex):
        for w in self.neighbours(vertex):
            for v in self.neighbours(w) - set([vertex]):
                if not self.separates([vertex],[v],[w]):
                    return True
        return False
    
                        
