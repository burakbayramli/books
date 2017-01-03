from gPy.IO import read_csv
from gPy.Parameters import CompactFactor
from gPy.Demos import marginalise_gui

cancer = CompactFactor(read_csv(open('cancer.dat')))
#create a normal factor
data = cancer['Smoker', 'Cancer', 'Bronchitis']
marginalise_gui(data)






