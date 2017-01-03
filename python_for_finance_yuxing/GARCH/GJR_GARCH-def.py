def GJR_GARCH(ret):
    startV=array([ret.mean(),ret.var()*0.01,0.03,0.09,0.90])
    finfo=np.finfo(np.float64)
    t=(0.0,1.0)
    bounds=[(-10*ret.mean(),10*ret.mean()),(finfo.eps,2*ret.var()),t,t,t]
    T=size(ret,0)
    sigma2=np.repeat(ret.var(),T)
    inV=(ret,sigma2)
    return fmin_slsqp(gjr_garch_likelihood,startV,f_ieqcons=gjr_constraint,bounds=bounds,args=inV)
