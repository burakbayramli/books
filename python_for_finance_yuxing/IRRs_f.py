from math import copysign
from scipy import mean
def npv_f(rate, cashflows):
    total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 +rate)**i
    return total
def IRRs_f(cash_flows):
    n=1000
    r=range(1,n)
    epsilon=abs(mean(cashflows)*0.01)
    irr=[-99.00]
    j=1
    npv=[]
    for i in r: npv.append(0)
    lag_sign=copysign(npv_f(float(r[0]*1.0/n*1.0),cash_flows),npv_f(float(r[0]*1.0/n*1.0),cash_flows))
    for i in range(1,n-1):
        interest=float(r[i]*1.0/n*1.0)
        npv[i]=npv_f(interest,cash_flows)
        s=copysign(npv[i],npv[i])
        if s*lag_sign<0:
            lag_sign=s
            if j==1:
                irr=[interest]
                j=2
            else:
                irr.append(interest)
    return irr
