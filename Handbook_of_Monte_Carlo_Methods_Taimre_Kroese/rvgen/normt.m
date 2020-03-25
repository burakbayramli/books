function out=normt(mu,sig,a,b)
pb=normcdf((b-mu)./sig);
pa=normcdf((a-mu)./sig);
C=pb-pa;
out=mu+sig.*norminv(C.*rand(size(mu))+pa);