import pandas as pd
import datetime
file=open("c:/temp/F-F_Research_Data_Factors_daily.txt","r")
data=file.readlines()
f=[]
Chapter 12
[ 353 ]
index=[]
for i in range(5,size(data)):
t=data[i].split()
t0_n=int(t[0])
y=int(t0_n/10000)
m=int(t0_n/100)-y*100
d=int(t0_n)-y*10000-m*100
index.append(datetime.datetime(y,m,d))
for j in range(1,5):
k=float(t[j])
f.append(k/100)
n=len(f)
f1=np.reshape(f,[n/4,4])
ff=pd.DataFrame(f1,index=index,columns=['Mkt_Rf','SMB','HML','Rf'])
ff.to_pickle("c:/temp/ffDaily.pickle")
