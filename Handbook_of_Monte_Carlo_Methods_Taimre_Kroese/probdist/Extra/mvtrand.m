function x=mvtrand(nu,mu,Sigma,N)
% multivariate t_nu(mu,Sigma) generator of 'N' random variables
% (Algorithm 4.72)

A=chol(Sigma); mu=mu(:);
n=length(mu);x=nan(N,n);
for i=1:N
    Y=randn(1,n)*sqrt(nu/gamrand(nu/2,1/2));
    x(i,:)=mu'+Y*A;
end