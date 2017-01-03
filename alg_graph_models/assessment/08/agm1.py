"""
This is the easy question. To construct the BN a student can either
deduce it from the information given or use a brute-force search to
find a BN which satistfies all the conditions. The latter option is
possible due to the small number of variables.

The 'ci' function is a direct implementation of what is discussed in
lectures.  Students who find the 'ancestral_adg' method in the gPy
Python package will find their job easier.
"""

from gPy.Graphs import ADG


_vertices = ['G'+str(i) for i in range(1,7)] 

_arrows = (
    ('G1','G2'),
    ('G1','G3'),
    ('G2','G4'),
    ('G3','G4'),
    ('G4','G6'),
    ('G4','G5')
    )
           
bnq1 = ADG(_vertices,_arrows)

def ci(adg,a,b,s):
    """Return whether a is independent of b given s in adg
    """
    return adg.ancestral_adg(a|b|s).moralise().separates(a,b,s)
