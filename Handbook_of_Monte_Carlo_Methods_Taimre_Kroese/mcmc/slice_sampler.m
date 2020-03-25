%slice_sampler.m
lam=1;alpha=2; a=1;b=6;
x=a; T=10^4; data=nan(T,1);
for t=1:T
   u=rand(1,2); 
   Up=min(x-log(u(2))/lam,b);
   Lo=max(x*u(1)^(1/(alpha-1)),a);
   x=rand*(Up-Lo)+Lo;
   data(t)=x;
end
% plot density estimate and comapre with true one
[bandwidth,density,xmesh]=kde(data,2^13,a,b);
plot(xmesh,density),hold on
C=gamcdf(b,alpha,1/lam)-gamcdf(a,alpha,1/lam);
plot(xmesh,gampdf(xmesh,alpha,1/lam)/C,'r')






