% copula.m
clear all, N = 10^4; nu=10;
Sig=[1,.7;.7,1]; % correlation matrix
A=chol(Sig);
% generate multivariate Student dist.
Y=repmat(sqrt(nu./gamrnd(nu/2,2,[N,1])),1,2).*randn(N,2);
Y=Y*A;
U = tcdf(Y,nu); % a sample from C(u_1,...,u_n)
X=[gaminv(U(:,1),2,1), norminv(U(:,2))];
plot(X(:,1),X(:,2),'r.','MarkerSize',1)

