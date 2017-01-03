import numpy

from particle import Particle

class ElementaryParticle(Particle):

    def __init__(self, spin):
        self.s = spin
        self.is_fermion = bool(spin%1.0)
        self.is_boson = not self.is_fermion
        self.constituents = None
