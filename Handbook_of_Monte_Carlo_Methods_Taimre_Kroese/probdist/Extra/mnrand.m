function x=mnrand(n,p)
% Mnom(n,p) generator using binomial method (Algorithm 4.70)

s=0;q=1;t=1;k=length(p);x=nan(1,k);
while t<=k
    x(t)=binomrnd_beta(n-s,p(t)/q);
    s=s+x(t);q=q-p(t);t=t+1;
end