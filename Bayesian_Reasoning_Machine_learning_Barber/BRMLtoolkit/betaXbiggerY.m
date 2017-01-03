function [p logp]=betaXbiggerY(a,b,c,d)
%BETAXBIGGERY  p(x>y) for x~Beta(a,b), y~Beta(c,d)
% [p logp]=betaXbiggerY(a,b,c,d)
del=0.0001; x=0:del:1;
p = del*sum(x.^(a-1).*(1-x).^(b-1).*betainc(x,c,d))/beta(a,b);
m=log(del)+(a-1)*log(x)+(b-1)*log(1-x)+log(betainc(x,c,d))-betaln(a,b);
logp = logsumexp(m);