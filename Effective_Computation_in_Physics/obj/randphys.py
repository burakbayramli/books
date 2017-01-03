import numpy as np 

class RandomPhysics(object):

    def __init__(self):
        self.colors = ["red", "green", "blue"]
        self.flavors = ["charm", "strange", "up", "down", "top", "bottom"]

    def color(self):
        return self.colors[np.random.randint(len(self.colors))]

    def flavor(self):
        return self.flavors[np.random.randint(len(self.flavors))]

