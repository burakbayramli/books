""" snippets.py """ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print ("Hello World!")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print ("Name :{1} ( height {2} m, age {0})". format (111 ," Bilbo " ,0.84))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = [1,'string '," another string "] # Quote type is not important
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = [1,2]
x[0] = 2  # Note that the first index is 0
x
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#x = (1 ,2)
#x[0] = 2
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a = [2, 3, 5, 7, 11, 13, 17, 19, 23] 
a[1:4]
a[:4]
a[3:]
a[-2:]
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'hello ' + 'world '  # String concatenation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'hello ' * 2 # String repetition
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[1 ,2] * 2 # List repetition
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
15 % 4 # Remainder of 15/4
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s = "hello"
d = dir(s)
print (d, flush = True) # Print the list in " flushed " format
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s = "hello"
help(s.replace)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s = 'hello'
s1 = s.replace ('e','a')
print(s1)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
t1 = type([1 ,2 ,3])
t2 = type((1 ,2 ,3))
t3 = type({1 ,2 ,3})
print(t1 ,t2 ,t3)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = [1 ,2]
y = x # y refers to the same object as x
print (id(x) == id(y)) # check that the object id's are the same
y[0] = 100 # change the contents of the list that y refers to
print (x)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = [1 ,2]
y = x # y refers to the same object as x
y = [100 ,2] # now y refers to a different object
print (id(x) == id(y))
print (x)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = [1 ,2 ,3]
def change_list (y):
    y.append(100) # Append an element to the list referenced by y
    y [0]=0 # Modify the first element
            # of the list referenced by y
    y = [2 ,3 ,4] # The local y now refers to a different list
                  # The list to which y first referred does not change
    return sum(y)
print (change_list(x))
print (x)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from numpy import array , square , sqrt
x = array ([1.2 ,2.3 ,4.5])

def stat(x):
    n = len(x) #the length of x
    meanx = sum(x)/n
    stdx = sqrt(sum( square (x - meanx ))/n)
    return [meanx ,stdx]

print (stat(x))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import numpy as np
np.sqrt(2)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from numpy import sqrt , cos
sqrt(2)
cos(1)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import numpy as np
ans = 'y'
while ans != 'n':
    outcome = np.random.randint(1,6+1)
    if outcome == 6:
        print ("Hooray a 6!")
        break
    else:
        print ("Bad luck, a", outcome )
    ans = input ("Again ? (y/n)")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s = "Hello"
for c in s:
    print (c,'*', end=' ')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s = "Hello"
t = s.__iter__ ()
# t is now an iterator . Same as iter(s)
print (t.__next__ () ) # same as next(t)
print (t.__next__ () )
print (t.__next__ () )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i in range (4 ,20):
    print (i, end=' ')
print ( range (4 ,20))        
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A = {3, 2, 2, 4}
B = {4, 3, 1}
C = A & B
for i in A:
    print (i)
print (C)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setA = {3, 2, 4, 2}
setB = {x**2 for x in setA}
print (setB)
listA = [3, 2, 4, 2]
listB = [x**2 for x in listA ]
print ( listB )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DICT = {'Gimly': 140 , 'Frodo':51 , 'Aragorn': 88}
for key in DICT:
    print (key , DICT[key])
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
class shire_person:
    def __init__ (self ,name): # initialization method
        self.name = name       # instance attribute
        self.age = 0           # instance attribute
    address = 'The Shire'      # class attribute

print (dir(shire_person)[1:5] , '...',dir(shire_person)[-2:])
                                    # list of class attributes

p1 = shire_person ('Sam')   # create an instance
p2 = shire_person ('Frodo') # create another instance
print (p1.__dict__)         # list of instance attributes

p2.race = 'Hobbit' # add another attribute to instance p2
p2.age = 33        # change instance attribute
print (p2.__dict__)

print (getattr(p1 ,'address')) # content of p1's class attribute
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
class person:
    def __init__ (self ,name):
        self.name = name
        self.age = 0
        self. address = ' '

class shire_person(person):
    def __init__ (self, name):
        super().__init__(name)
        self.Shire_address = 'Bag End'
        
p1 = shire_person("Frodo")
p2 = person ("Gandalf")
print (dir(p1)[:1] , dir(p1)[ -3:] )
print (dir(p2)[:1] , dir(p2)[ -3:] )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
fout = open('output.txt ','w')
for i in range (0 ,41):
    if i%10 == 0:
        fout.write('{:3d}\n'. format (i))
fout.close ()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
'{:5d}'.format(123)
'{:.4e}'.format(1234567890)
'{:.2f}'.format(1234567890)
'{:.2f}'.format(2.718281828)
'{:.3f}'.format(2.718281828)
'{:.3g}'.format(2.718281828)
'{:.3e}'.format(2.718281828)
'{0:3.3f}; {1:.4e};'.format(123.456789, 0.00123456789)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
fin = open('output.txt ','r')
for line in fin:
    line = line. strip () # strips a newline character
    print (line)
fin.close ()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
with open('output.txt', 'w') as f:
    f.write ('Hi there!')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
numline = 0
DICT = {}
with open('ataleof2cities.txt', encoding ="utf8") as fin:
    for line in fin:
        words = line.split()
        for w in words:
            if w not in DICT:
                DICT[w] = 1
            else:
                DICT[w] +=1
        numline += 1
        
sd = sorted(DICT, key=DICT.get, reverse =True)   #sort the dictionary
print("Number of unique words : {}\n". format (len(DICT)))
print("Ten most frequent words :\n")
print("{:8} {}". format ("word", " count "))
print(15* '-')
for i in range (0 ,10):
    print ("{:8} {}". format (sd[i], DICT[sd[i]]))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
import numpy as np # import the package
x = np.cos(1)
data = [1 ,2 ,3 ,4 ,5]
y = np.mean(data)
z = np.std(data)
print ('cos(1) = {0:1.8f} mean = {1} std = {2} '.format(x,y,z))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
A = np.zeros([2 ,3 ,2]) # 2 by 3 by 2 array of zeros
print(A)
print(A.shape) # number of rows and columns
print(type(A)) # A is an ndarray
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
a = np.array(range (4)) # equivalent to np. arange (4)
b = np.array([0 ,1 ,2 ,3])
C = np.array([[1 ,2 ,3] ,[3 ,2 ,1]])
print(a, '\n', b,'\n' , C)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
a = np.array(range (9)) #a is an ndarray of shape (9 ,)
print (a.shape )
A = a.reshape(3 ,3)     #A is an ndarray of shape (3 ,3)
print(a)
print(A)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
a = np.arange(3) #1D array ( vector ) of shape (4 ,)
print(a)
print(a.shape)
b = a.reshape( -1 ,1) # 4x1 array ( matrix ) of shape (4 ,1)
print (b)
print (b.T)
A = np. arange (9). reshape (3 ,3)
print (A.T)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
A = np.ones((3 ,3))
B = np.zeros((3 ,2))
C = np.hstack((A,B))
print(C)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
A = np.array ( range (9)).reshape (3 ,3)
print(A)
print(A[0])    # first row
print(A[: ,1]) # second column
print(A[0 ,1]) # element in first row and second column
print(A [0:1 ,1:2]) # (1 ,1) ndarray containing A[0 ,1] = 1
print(A[1:,-1]) # elements in 2nd and 3rd rows , and last column
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
A[1: ,1] = [0 ,0] # change two elements in the matrix A above
print(A)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
x = np.array([[2 ,4] ,[6 ,8]])
y = np.array([[1 ,1] ,[2 ,2]])
print(x+y)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print (np. divide (x,y)) # same as x/y
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print (np.sqrt(x))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(np.dot(x,y))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(x.dot(x)) # same as np.dot(x,x)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(x @ y)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
import numpy as np
A= np.arange(4).reshape (2 ,2) # (2 ,2) array
x1 = np.array([40 ,500])        # (2 ,) array
x2 = x1.reshape(2 ,1)           # (2 ,1) array
print(A + x1) # shapes (2 ,2) and (2 ,)
print(A * x2) # shapes (2 ,2) and (2 ,1)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
B = np.arange(8).reshape (2 ,2 ,2)
b = np.arange(4).reshape (2 ,2)
print (B@b)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
a = np.array(range (4)).reshape (2 ,2)
print(a.sum(axis =0)) # summing over rows gives column totals
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
import numpy as np
np.random.seed (123)        # set the seed for the random number generator
x = np.random.random()      # uniform (0 ,1)
y = np.random.randint(5,9)  # discrete uniform 5 ,... ,8
z = np.random.randn(4)      # array of four standard normals
print(x,y,'\n',z)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
import pandas as pd  
DICT = {'one':1, 'two':2, 'three':3, 'four':4}
print(pd.Series(DICT))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
years = ['2000','2001','2002']
cost = [2.34 , 2.89 , 3.01]
print(pd.Series(cost, index = years, name = 'MySeries')) #name it
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
DICT = {'numbers':[1 ,2 ,3 ,4] , 'squared':[1 ,4 ,9 ,16] }
df = pd.DataFrame (DICT, index = list('abcd'))
print (df)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
ages = [6, 3, 5, 6, 5, 8, 0, 3]
d={'Gender':['M', 'F']*4 , 'Age': ages}
df1 = pd.DataFrame (d)
df1.at[0,'Age']= 60 # change an element
df1.at[1,'Gender'] = 'Female ' # change another element
df2 = df1.drop('Age' ,1) # drop a column
df3 = df2.copy() # create a separate copy of df2
df3['Age'] = ages # add the original column
dfcomb = pd.concat([df1, df2, df3],axis =1) # combine the three dfs
print (dfcomb)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
d={'Gender':['M', 'F', 'F']*4 , 'Age': [6, 3, 5, 6, 5, 8, 0, 3, 6, 6, 7, 7]}
df=pd.DataFrame(d)
print (df.dtypes)
df['Gender'] = df['Gender'].astype('category') # change the type
print(df.dtypes)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
import numpy as np
import pandas as pd
ages = [6, 3, 5, 6, 5, 8, 0, 3]
np.random.seed(123)
df = pd.DataFrame(np.random.randn(3, 4), index = list('abc'), 
                      columns = list('ABCD'))
print(df)
df1 = df.loc["b":"c","B":"C"] # create a partial data frame
print(df1)
meanA = df['A'].mean() # mean of 'A' column
print('mean of column A = {}'.format(meanA))
expA = df['A'].apply(np.exp) # exp of all elements in 'A' column
print(expA)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
df = pd.DataFrame({'W':['a','a','b','a','a','b'],
                   'X':np.random.rand(6) ,
                   'Y':['c','d','d','d','c','c'], 
                   'Z':np.random.rand(6) })
print(df)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(df.groupby('W').mean())
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(df.groupby(['W','Y']).mean ())
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
print(df.groupby('W').agg ([sum ,np.mean]))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
import numpy as np
import pandas as pd
import matplotlib
df = pd.DataFrame({'normal ':np.random.randn(100),
                   'Uniform':np.random.uniform (0 ,1 ,100) })
font = {'family' : 'serif', 'size' : 14} #set font
matplotlib.rc('font', ** font) # change font
df.plot() # line plot ( default )
df.plot(kind = 'box') # box plot
matplotlib.pyplot.show () # render plots
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
#from sklearn.model_selection import train_test_split
#X_train , X_test , y_train , y_test = train_test_split (X, y, test_size = 0.5)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
#from sklearn import preprocessing
#min_max_scaler = preprocessing.MinMaxScaler( feature_range =(0 , 1))
#x_scaled = min_max_scaler.fit_transform(X) # equivalent to:
#x_scaled = (X - X.min(axis =0)) / (X.max(axis =0) - X.min(axis =0))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
#from sklearn.someSubpackage import someClassifier
#clf = someClassifier () # choose appropriate classifier
#clf.fit(X_train , y_train ) # fit the data
#y_prediction = clf.predict( X_test ) # predict
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
#from sklearn import svm
#clf = svm.SVC( kernel = 'rbf')
#clf.fit(X_train, y_train)
#y_prediction = clf.predict( X_test )
#from sklearn.metrics import confusion_matrix
#print(confusion_matrix(y_test, y_prediction ))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    



