function c_0=h(U,N,n,T,r,sig,K,S_0,dt)
X=nan(N,1); % number of sample path simulations
for i=1:N
    path=(r-sig^2/2)*dt+sig*sqrt(dt)*norminv(U(i,:));
    path=cumprod([S_0,exp(path)]);
    X(i)=exp(-r*T)*max(mean(path)-K,0);
end
c_0=mean(X);