%sumlognor.m
clear all,clc
n=10; gamma=4*10^4; N=10^5; param=1:n;
p = 1-logncdf(gamma,param-10,sqrt(param)); p=p/sum(p);
J = randsample(n,N,'true',p); %sample index J
% generate r.v.'s X_1,X_2,...
X = lognrnd(param(ones(N,1),:)-10,sqrt(param(ones(N,1),:)));
% set the J-th entry in each row to 0.
X((J'-1)*N+(1:N))=0;
% compute estimator
Y=1-logncdf( max( [gamma-sum(X,2),X],[],2 ),J-10,sqrt(J) );
Y=Y'./p(J);
mean(Y), std(Y)/sqrt(N)/mean(Y)
