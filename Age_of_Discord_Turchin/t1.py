import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

data = pd.read_csv('PSImodel2020.csv',index_col=0)

relWage1 = data['ProdWage'] / data['GDPpc']
relWage1 = relWage1/relWage1.loc[1980]
relWage2 = data['UnskillWage'] / data['GDPpc']
relWage2 = relWage2/relWage2.loc[1980]
data['RelWage'] = (relWage1 + relWage2) / 2

