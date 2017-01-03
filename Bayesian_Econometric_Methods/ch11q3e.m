%Matlab code for part e) Question 3 of Chapter 11
%This program calculates posterior means and standard deviations
%for regressions coefficients using both analytical methods and 
%Monte Carlo integration
%Note that I am drawing from the t-distribution for beta
%the function tdis_rnd.m does this, which I got 
%from James LeSage's Econometrics Toolbox (www.spatial-econometrics.com/)


%Load in the data you generated in part a)
load ch2q2.out;
n=size(ch2q2,1);
y=ch2q2(:,1);
x=ch2q2(:,2:3);
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
capv0inv=inv(capv0);
%Ordinary least squares quantities
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
v=n-k;

%Posterior hyperparameters for Normal-Gamma
xsquare=x'*x;
v1=v0+n;
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

vscale = s12*capv1;
vchol=chol(vscale);
vchol=vchol';

%Now start Monte Carlo loop
%beta is t(b1,vscale,v1)
b2mean=zeros(k,1);
b2square=zeros(k,1);

%Specify the number of replications
r=10000;

%
%tdis_rnd takes random draws from the multivariate t
%with mean zero and scale, V=I
%Hence we have to transform draws to get t(b1,bscale,v1)
for i = 1:r
    %draw a t(0,1,v1) then transform to yield draw of beta
    bdraw=b1 + vchol*tdis_rnd(k,v1);
    b2mean=b2mean+bdraw;  
    b2square=b2square+bdraw.^2; 
end

b2mean=b2mean./r;
b2square=b2square./r;
b2var=b2square - b2mean.^2;
b2sd=sqrt(b2var);

nse=b2sd./sqrt(r);

%Print out whatever you want
'Hyperparameters for informative natural conjugate prior'
b0
capv0
v0
s02

'Posterior results based on Informative Prior'
b1
bsd
b2mean
b2sd
nse