from steelpy import Spreadsheet

ss = Spreadsheet()
#
# basic functionality
#
from math import sin, pi
ss.tools.update(sin=sin, pi=pi, len=len)
ss['A1'] = 5
ss['A2'] = '=A1*6'
ss['A3'] = '=A2*7'
print(ss['A1'], ss['A2'], ss['A3'])
#
ss['B1'] = '=sin(pi/4)'
print(ss['B1'])
#
# read existing spreadsheet
#
wb = ss.read_book("Clair Caisson C3 Test Data HW v0 25-Aug-2020.xlsx")
sheets = wb.sheet_names
print(sheets)
#
ws = wb.sheets["Caisson Supports"]
#
# get data as dataframe
#
data = ws.to_df()
data.index = ['memb1', 'memb2', 'memb3', 'memb4', 'memb5']
print(data)
item = data.y
print(item.max())
print(item)
#
print('---')
for row in data.itertuples():
   print(f"{row}")
#
index = item.idxmax()
for index, row in data.iterrows():
   print( f"{index}: {row['NodeNo']}")
   print(row)
#
#
#print(data.columns)
idx = len(data.columns)
#
# Select Rows Between two Index Labels
yy = data.loc['memb1':'memb4']
print(yy)
# Select Single Column by label
yy = data.loc[:, 'Support Fixity']
print(yy)
# Select Multiple Rows by Label
yy = data.loc[['memb1', 'memb2', 'memb3']]
print(yy)
# Select Multiple Columns by labels
yy = data.loc[:, ['x','y','z']]
print(yy)
# Select Columns between two Labels
yy = data.loc[:, 'x':'z']
print(yy)
# Select Alternate rows By indeces
yy = data.loc[0:3:2]
print(yy)
# Select Alternate Columns between two Labels
yy = data.loc[:, 'x':'z':2]
print(yy)
#
yy = data.loc[['memb1', 'memb2', 'memb3'], ['x','y','z']]
print(yy)
#
yy = data.loc['memb1', 'y']
print(yy)
yy = data.loc['memb1']
print(yy)
#
print('---')
xx = data.iloc[0]
print(xx)
print('---')
print(data.iloc[[0]])
#
print('---')
print(data.iloc[:3])
print('---')
print(data.iloc[:3,:idx].astype('str'))
#
print(data.tabulate())
#
#print(data.to_string())
#
#gby = data.groupby(['Support_Type', 'SupportNo', 'Support_Fixity'])
gby = data.groupby('Support Type')
groups = gby.groups
print(groups)
#
getgroup = gby.get_group("Guide")
print(getgroup.tabulate()) # 
#
print('--')