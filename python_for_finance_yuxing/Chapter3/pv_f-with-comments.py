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
	
