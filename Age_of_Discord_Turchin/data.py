import pandas as pd

df = pd.read_excel("PSImodel2020.xlsx")
df.to_csv('PSImodel2020.csv',index=None)
print (df)
