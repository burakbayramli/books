function X=wishrand(r,Sigma)
% Wishart(r,Sigma) generator via Bartlett decomposition
% (Algorithm 4.74)

C=chol(Sigma)'; n=length(C);
for i=1:n
    y(i)=gamrand((r-i+1)/2,1/2);
end
A=diag(sqrt(y));
d=ones(n*(n-1)/2,1);
d(1+cumsum(0:n-2))=n+1:-1:3;
d = cumsum(d);
A(d)=randn(n*(n-1)/2,1);
A=A';
X=C*A*A'*C';