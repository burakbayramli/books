import numpy as np
import p4f as pf
s0=30;T0=0.5;sigma0=0.2;r0=0.05;x0=30
sigma=np.arange(0.5,2.0,0.5)
call_0=pf.bs_call(s0,x0,T0,r0,sigma)
call_sigma=pf.bs_call(s0,x0,T0,r0,sigma)
call_T=pf.bs_call(s0,x0,Tr0,sigma0)
plot(sigma,call_sigma,'b')
plot(T,call_T)
