from gPy.IO import read_dnet
from gPy.Models import BNM
from gPy.Demos import marginalise_gui

asia = BNM()
asia.from_dnet(read_dnet(open('Asia_lower.dnet')))
smoking = asia['Smoking'] * 1
cancer = asia['Cancer'] * 1
bronchitis = asia['Bronchitis'] * 1
smoking._data = [0.4,0.6] # gives better example

print cancer
print smoking
print bronchitis
foo = bronchitis.demo_mult(cancer)
joint = foo.demo_mult(smoking)
print joint
#show joint as a single factor with strings not numbers
#then marginalise joint, again showing numbers





