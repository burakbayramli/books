function x=mvnrand(mu,Sigma,N)
% multivariate N(mu,Sigma) generator of 'N' random variables
% (Algorithm 4.71)

A=chol(Sigma); mu=mu(:);
n=length(mu);x=nan(N,n);
for i=1:N
    x(i,:)=mu'+randn(1,n)*A;
end

