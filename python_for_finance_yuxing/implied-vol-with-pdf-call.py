import p4f
S=40
K=40
T=0.5
r=0.05
c=3.30
for i in range(200):
    sigma=0.005*(i+1)
    diff=c-p4f.bs_call(S,K,T,r,sigma)
    if abs(diff)<=0.01:
        print(i,sigma,diff)
