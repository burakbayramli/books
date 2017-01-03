''' Visualize sample data
Data have been taken from the "Journal of Statistics Education"
http://www.amstat.org/publications/jse/jse_data_archive.htm

'''

'''
Author:  Thomas Haslwanter
Date:    Jan-2014
Version: 1.0
'''
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import os

# Get the data
dataDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Data\amst'
inFile = os.path.join(dataDir, 'babyboom.dat.txt')
df = pd.read_csv(inFile, sep='[ ]*', header=None)

# Select the required columns
data = df[[1,2,3]]
data.columns = ['nSex', 'weight', 'minutes']

# Gender column "male/female"
data['sex'] = 'female'
data['sex'][data['nSex']==2] = 'male'
data = data.drop('nSex', axis=1)

# Show a boxplot
data[['weight', 'sex']].boxplot(by='sex')
plt.show()
sns.boxplot(data['weight'], groupby=data['sex'])
plt.show()

# Show a regplot
data[['weight', 'minutes']] = data[['weight', 'minutes']].astype(np.double)
sns.regplot('minutes', 'weight', data=data)
plt.show()



