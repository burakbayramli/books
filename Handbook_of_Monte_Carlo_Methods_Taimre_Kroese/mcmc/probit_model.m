%probit_model.m
clear all,clc
n=5000; % number of data points (y_1,...,y_n)
k=3;   % number of explanatory variables
% generate artificial dataset
randn('seed', 12345);  rand('seed', 67890);
truebeta = [1 -5.5 1]';
X = [ ones(n,1) randn(n,k-1)*0.1 ];  % design matrix
Y = binornd(1,normcdf(X*truebeta));
bo=zeros(k,1); % we set Vo=100*eye(k);
I1=find(Y==1); Io=find(Y==0);
Y_star=randn(n,1);
%compute the Cholesky decomp. to avoid using inv.m
L=chol(eye(k)/100+X'*X);
T=10^4; data=nan(T,k); %allocate memory
for t=1:T
    % sample beta given Y^*
    b=L\(L'\(bo/100+X'*Y_star))+L\randn(k,1);
    % sample Y^* given beta
    M=X*b;
    Y_star(I1)=normt(M(I1),1,0,inf);
    Y_star(Io)=normt(M(Io),1,-inf,0);
    data(t,:)=b';
end
b_hat=mean(data)
Cov_hat=cov(data)

