from gPy.IO import read_csv
from gPy.Parameters import CompactFactor
import gPy.Parameters


cancer = CompactFactor(read_csv(open('cancer.dat')))
#create a normal factor
table = cancer['Smoker', 'Cancer', 'Bronchitis']
print table
print 'Number of observations is %d' % table.z()
gPy.Parameters.precision = 6
print table.normalised()
