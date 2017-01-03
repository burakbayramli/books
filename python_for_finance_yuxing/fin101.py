def fin101():
"""
1) Basic functions:
PV: pv_f,pv_annuity, pv_perpeturity
FV: fv_f, fv_annuity, fv_annuity_due
2) How to use pv_f?
>>>help(pv_f)
"""
def pv_f(fv,r,n):
	"""
	Objective: estimate present value 
		fv: future value
		r : discount periodic rate
		n: number of periods
	formula : fv/(1+r)**n
	e.g.,
	>>>pv_f(100,0.1,1)
	90.90909090909090909090909
	>>>pv_f(r=0.1,fv=100,n=1)
	90.90909090909090909090909
	>>>pv_f(f=1,fv,100,r=0.1)
	90.90909090909090909090909
	"""
	return fv/(1+r)**n
	
#present value of perpetuity
def pv_perpetuity(c,r):
	# c is cash flow
	# r is discount rate
	return c/r

def pv_growing_perpetuity(c,r,g):
	if(r<g):
		print("r<g !!!!")
	else:
		return(c/(r-g))

def npv_f(rate, cashflows):
          total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 + rate)**1
    return total
