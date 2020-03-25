%rpcauchy.m
Delta=10^(-5); N=10^5; times=(0:1:N).*Delta;
Z=randn(1,N+1)./randn(1,N+1); 
Z=Delta.*Z; Z(1)=0; 
X=cumsum(Z);
plot(times,X)

