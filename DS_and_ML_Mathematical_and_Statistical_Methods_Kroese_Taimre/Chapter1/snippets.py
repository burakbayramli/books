""" snippets.py """ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import pandas as pd
abalone = pd.read_csv('abalone.data',header = None)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
urlprefix = 'http://vincentarelbundock.github.io/Rdatasets/csv/'
dataname = 'datasets/iris.csv'
iris = pd.read_csv( urlprefix + dataname )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iris.head()
iris = iris.drop('Unnamed: 0' ,1)
abalone.head(3)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abalone.columns = ['Sex', 'Length', 'Diameter', 'Height',
'Whole weight','Shucked weight', 'Viscera weight', 'Shell weight',
'Rings']
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xls = 'http://www.biostatisticien.eu/springeR/nutrition_elderly.xls'
nutri = pd.read_excel(xls)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pd.set_option('display.max_columns', 8) # to fit display
nutri.head(3)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nutri.info()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DICT = {1: 'Male', 2:'Female'} # dictionary specifies replacement
nutri['gender'] = nutri['gender'].replace (DICT)
nutri['gender'] = nutri['gender'].astype ('category')
nutri['height'] = nutri['height'].astype (float)
nutri.to_csv('nutri.csv',index = False)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nutri = pd.read_csv('nutri.csv')
nutri['fat'].describe()
nutri['fat'].value_counts()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pd.crosstab(nutri.gender, nutri.situation)
pd.crosstab(nutri.gender, nutri.situation, margins =True)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nutri['height'].mean ()
nutri['height'].quantile (q =[0.25 ,0.5 ,0.75])
nutri['height'].max() - nutri['height'].min ()
round( nutri ['height'].var(), 2) # round to two decimal places
round( nutri ['height'].std(), 2)
nutri['height'].describe ()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
width = 0.35 # the width of the bars
x = [0, 0.8 , 1.6] # the bar positions on x-axis
situation_counts = nutri ['situation'].value_counts ()
plt.bar(x, situation_counts , width , edgecolor = 'black')
plt.xticks(x, situation_counts.index)
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.boxplot(nutri['age'], widths = width ,vert= False)
plt.xlabel('age')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
weights = np.ones_like (nutri.age)/ nutri.age.count ()
plt.hist(nutri.age ,bins =9, weights=weights , facecolor ='cyan',
             edgecolor ='black', linewidth =1)
plt. xlabel ('age')
plt. ylabel ('Proportion of Total')
plt.show ()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = np.sort(nutri.age)
y = np.linspace (0,1, len(nutri.age))
plt.xlabel ('age')
plt.ylabel ('Fn(x)')
plt.step(x,y)
plt.xlim(x.min() ,x.max())
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import seaborn as sns
sns.countplot (x='situation', hue = 'gender', data=nutri,
              hue_order = ['Male', 'Female'], palette = ['SkyBlue','Pink'],
              saturation = 1, edgecolor ='black')
plt.legend (loc='upper center')
plt.xlabel ('')
plt.ylabel ('Counts')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.scatter(nutri .height , nutri .weight , s=12 , marker ='o')
plt.xlabel('height')
plt.ylabel('weight')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
urlprefix = 'http://vincentarelbundock.github.io/Rdatasets/csv/'
dataname = 'MASS/birthwt.csv'
bwt = pd.read_csv(urlprefix + dataname)
bwt = bwt.drop('Unnamed: 0' ,1)
#drop unnamed column
styles = {0: ['o','red'], 1: ['^','blue']}
for k in styles:
    grp = bwt[bwt.smoke == k]
    m,b = np.polyfit(grp.age , grp.bwt , 1) # fit a straight line
    plt.scatter(grp.age , grp.bwt , c= styles [k][1] , s=15 , linewidth =0,
                 marker = styles[k][0])
    plt.plot(grp.age , m*grp.age + b, '-', color = styles[k][1])

plt.xlabel('age')
plt.ylabel('birth weight (g)')
plt.legend(['non - smokers','smokers'],prop ={'size':8},loc =(0.5 ,0.8))
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
males = nutri[nutri.gender == 'Male']
females = nutri[nutri.gender == 'Female']
plt.boxplot([males.coffee, females.coffee ], notch =True , widths=(0.5 ,0.5))
plt.xlabel ('gender')
plt.ylabel ('coffee')
plt.xticks ([1 ,2] ,['Male','Female'])
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



