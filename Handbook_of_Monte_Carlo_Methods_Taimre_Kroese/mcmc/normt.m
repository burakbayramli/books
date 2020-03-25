function out=normt(mu,sig,a,b)
% draws from the conditional  pdf
% phi((x-mu)/sigma)/sigma * I(a<x<b),
% where phi(x) is the standard normal.
% Uses inverse method.

pb=normcdf((b-mu)./sig);
pa=normcdf((a-mu)./sig);

C=pb-pa;

out=mu+sig.*norminv(C.*rand(size(mu))+pa);