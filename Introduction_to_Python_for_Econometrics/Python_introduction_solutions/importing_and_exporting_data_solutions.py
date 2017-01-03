"""Solutions for 'Import and Exporting Data' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('importing_and_exporting_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
import numpy as np
import matplotlib.mlab as ml
import scipy.io as io
import xlrd
import openpyxl
# <demo> --- stop ---
# Exercise 1
# This exercise is doen in Excel or OpenOffice
# <demo> --- stop ---
# Exercise 2
import1 = np.genfromtxt('exercise3_numeric.csv',delimiter=',')
import2 = np.loadtxt('exercise3_numeric.csv',delimiter=',', skiprows=1)
import3 = ml.csv2rec('exercise3_numeric.csv')

import4 = np.genfromtxt('exercise3_numeric.txt',delimiter='\t')
import5 = np.loadtxt('exercise3_numeric.txt',delimiter='\t', skiprows=1)
# <demo> --- stop ---
# Exercise 3
dates = import2[:,0]
SP500 = import2[:,1]
XOM = import2[:,2]
# <demo> --- stop ---
# Exercise 4
np.savez('exercise3',dates=dates,SP500=SP500,XOM=XOM)
np.savez_compressed('exercise3_compressed',dates=dates,SP500=SP500,XOM=XOM)
mdict = {'dates':dates,'SP500':SP500,'XOM':XOM}
io.savemat('exercise3',mdict)
# <demo> --- stop ---
# Exercise 5
sumreturns = SP500+XOM
outputdata = np.hstack((dates,sumreturns))
# <demo> --- stop ---
# Exercise 6
np.savetxt('exercise3_new.csv',outputdata)
# <demo> --- stop ---
# Exercise 7
wb = xlrd.open_workbook('exercise3.xls')
sheetNames = wb.sheet_names()
# Assumes 1 sheet name
sheet = wb.sheet_by_name(sheetNames[0])
excelData = []
for i in xrange(sheet.nrows):
    excelData.append(sheet.row_values(i))

header = excelData[0]
import_excel = np.array(excelData[1:])
# <demo> --- stop ---
# Exercise 8
wb = openpyxl.load_workbook('exercise3.xlsx')
sheetNames = wb.get_sheet_names()
sheet = wb.get_sheet_by_name(sheetNames[0])
excelData = []
rows = sheet.rows
header = []
for c in rows[0]:
    header.append(c.value)

for r in rows[1:]:
    data = []
    for c in r:
        data.append(c.value)
    excelData.append(data)

import_xlsx = np.array(excelData)
