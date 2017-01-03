import randphys as rp 

class Quark(object):

    def __init__(self):
        phys = rp.RandomPhysics()
        self.color = phys.color()
        self.flavor = phys.flavor()

    def join_trio(self, brother, sister):
        self.friends_ = [brother, sister]

    def friends(self):
        return self.friends

    def color(self):
        return self.color

    def flavor(self):
        return self.flavor

    def flip(self):
        if self.flavor == "up":
            self.flavor = "down"
        elif self.flavor == "down":
            self.flavor = "up"
        elif self.flavor == "top":
            self.flavor = "bottom"
        elif self.flavor == "bottom":
            self.flavor = "top"
        elif self.flavor == "strange":
            self.flavor = "charm"
        elif self.flavor == "charm":
            self.flavor = "strange"
        else :
            raise AttributeError("The quark cannot be flipped, because the "
                                 "flavor is not valid.")