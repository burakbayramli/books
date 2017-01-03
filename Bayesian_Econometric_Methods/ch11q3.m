%Matlab code for Question 3 of Chapter 11

%Load in the data you generated in part a)-- relevant code to be found in ch11q3a.m
load ch2q2.out;
n=size(ch2q2,1);
y=ch2q2(:,1);
x=ch2q2(:,2:3);
%x=ch2q2(:,2);
k=size(x,2);
%Hyperparameters for natural conjugate prior
%I am using the suffix "0" to denote prior hyperparameters
v0=1;
s2inv0=1;
s02=1/s2inv0;
b0=zeros(k,1);
b0(k,1)=1;
c=1;
capv0=c*eye(k);

%Parameters for Normal-Gamma posterior
%First let's do OLS since some posterior results written in terms of OLS
%Ordinary least squares quantities
xsquare=x'*x;
bols = inv(xsquare)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
v=n-k;

%Now use OLS in the formulae for the posterior

v1=v0+n;
capv0inv = inv(capv0);
capv1inv = capv0inv+ xsquare;
capv1=inv(capv1inv);
b1 = capv1*(capv0inv*b0 + xsquare*bols);
v1s12 = v0*s02 + v*s2 + (bols-b0)'*inv(capv0 + inv(xsquare))*(bols-b0);
s12 = v1s12/v1;

bcov = capv1*v1s12/(v1-k);
bsd=zeros(k,1);
for i = 1:k
bsd(i,1)=sqrt(bcov(i,i));
end

%Now we can print out the posterior mean and standard deviation of beta
'posterior mean and standard deviation of beta'
[b1 bsd]

%log of marginal likelihood for the model if prior is informative
    intcon=gammaln(.5*v1) + .5*v0*log(v0*s02)- gammaln(.5*v0) -.5*n*log(pi);
    lmarglik=intcon + .5*log(det(capv1)/det(capv0)) - .5*v1*log(v1s12);
'log of marginal likelihood'
lmarglik