import numpy as np
import os
import pandas as pd

dataDir = r'C:\Users\p20529\Documents\Teaching\Master_FH\Stats\Data'
os.chdir(dataDir)

# Read a csv-file
data = pd.read_csv(r'Swimming\swimming_100m.csv')

# Read an Excel-file
inFile = r'data_dobson\GLM_data\Table 2.8 Waist loss.xls'
data = pd.read_excel(inFile, 'Sheet1', skiprows=2)

# Convert to array
dataArray = data.values
print(dataArray[1])

# Generate a dataframe
x = np.arange(0,10,0.1)
y = np.sin(x)
z = np.cos(x)

df = pd.DataFrame({'XVals':x, 'YVals':y, 'ZVals':z})
print(df.head())

data = df[['YVals', 'ZVals']][9:15]
print(data)