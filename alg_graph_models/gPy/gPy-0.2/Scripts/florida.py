from gPy.IO import read_csv
from gPy.Parameters import CompactFactor
import gPy.Parameters


florida = CompactFactor(read_csv(open('florida.dat')))
#create a normal factor
table = florida['Murderer', 'Sentence', 'Victim']
print table
print 'Number of observations is %d' % table.z()
gPy.Parameters.precision = 6
print table.normalised()
