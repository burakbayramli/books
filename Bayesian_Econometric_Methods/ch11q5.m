%Matlab code for Question 5 of Chapter 11
%This program calculates posterior means and standard deviations
%for regressions coefficients using both analytical methods and 
%Monte Carlo integration
%Note that I am drawing from the t-distribution for beta
%the function tdis_rnd.m does this, which I got 
%from James LeSage's Econometrics Toolbox (www.spatial-econometrics.com/)


%Load in the data you generated in part a) -- the program to do this is ch11q3a.m
load ch2q2.out;
n=size(ch2q2,1);
y=ch2q2(:,1);
x=ch2q2(:,2:3);
k=size(x,2);

%Ordinary least squares quantities
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
v=n-k;


%Begin by doing the prior sensitivity analysis analytically
%Focus is on slope coefficient, beta(2), so only keep results relating to that
%specify the range of values for c and d, the hyperparameters varied in sensitivity analysis
cgrid=[0; 1; 2];
ncgrid=size(cgrid,1);
dgrid =[.01; 1; 100];
ndgrid=size(dgrid,1);


%loop over all values of c and d
b2keep=[];
cdkeep=[];

for icgrid=1:ncgrid
c=cgrid(icgrid,1);
for idgrid=1:ndgrid
d=dgrid(idgrid,1);

%Hyperparameters for natural conjugate prior
v0=1;
s2inv0=1;
s02=1/s2inv0;
b0=zeros(k,1);
b0(k,1)=c;
capv0=eye(k);
capv0(2,2)=d;
capv0inv=inv(capv0);

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

b2keep=[b2keep; [b1(2,1) bsd(2,1)]];
cdkeep=[cdkeep; [c d]];
end 
end

vscale = s12*capv1;
vchol=chol(vscale);
vchol=vchol';

%reset the prior hyperparameter to Base Prior values
b0(k,1)=1;
capv0(2,2)=1;
capv1inv = capv0inv+ xsquare;
capv1=inv(capv1inv);
b1 = capv1*(capv0inv*b0 + xsquare*bols);
v1s12 = v0*s02 + v*s2 + (bols-b0)'*inv(capv0 + inv(xsquare))*(bols-b0);
s12 = v1s12/v1;

slopemeans=zeros(ncgrid*ndgrid,1);
slope2mos=zeros(ncgrid*ndgrid,1);
wsums=zeros(ncgrid*ndgrid,1);

%Specify the number of replications
r=100;

%tdis_rnd takes random draws from the multivariate t
%with mean zero and scale, V=I
%Hence we have to transform draws to get t(b1,bscale,v1)
for i = 1:r
    %draw a t(0,1,v1) then transform to yield draw of beta
    bdraw=b1 + vchol*tdis_rnd(k,v1);
    b2mean=b2mean+bdraw;  
    b2square=b2square+bdraw.^2;
    slopedraw=bdraw(2,1);
    %loop over grid of c and d values in prior sensitivity analysis
    icd=0;
    for icgrid=1:ncgrid
    c=cgrid(icgrid,1);
    for idgrid=1:ndgrid
    d=dgrid(idgrid,1);
     
    %Importance sampling weight
    w=tdens(slopedraw,c,d,v1)/tdens(slopedraw,b0(2,1),capv0(2,2),v1);
    icd=icd+1;
    slopemeans(icd,1)=slopemeans(icd,1) + w*slopedraw;
    slope2mos(icd,1)=slope2mos(icd,1) + w*slopedraw^2;
    wsums(icd,1)=wsums(icd,1) + w;
end
end
end

'Prior sensitivity results for beta(2)'
'c and d then Posterior mean and standard deviation -- analytical'
[cdkeep b2keep]

slopemeans=slopemeans./wsums;
slope2mos=slope2mos./wsums;
slopesds=sqrt(slope2mos - slopemeans.^2);

'c and d then Posterior mean and standard deviation -- importance sampling'
[cdkeep slopemeans slopesds]

