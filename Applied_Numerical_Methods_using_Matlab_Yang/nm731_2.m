%nm731_2 try using lsqnonlin() for a vector-valued objective ftn F(x)
clear, clf
N=3; a0=zeros(1,N); %the initial guess of polynomial coefficient vector
ao_lsq=lsqnonlin('f731_2',a0) %parameter estimate by lsqnonlin()
xx=-2+[0:400]/50;  fx=1./(1+8*xx.*xx);
ao_fit=polyfits(xx,fx,N-1) %parameter estimate by polyfits()
