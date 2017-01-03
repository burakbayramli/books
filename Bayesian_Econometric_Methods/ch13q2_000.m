%program which does empirical illustration for Exercise 2 of chapter 13
%Heteroskedasticity of known form
%Gibbs sampling for independent Normal-Gamma prior for beta and h
%load in the data set. Here use house price data from hprice.txt
%Note: this calls the script ch13q2w.m
%Note: this program gives you a choice of specifying the M-H variance or
%reading in a posterior variance for a previous run
load hprice.txt;
n=size(hprice,1);
y=hprice(:,1);
x=hprice(:,2:5);
x=[ones(n,1) x];
k=5;

%Specify explanatory variables for heteroskedasticity
z=x(:,2:k);
p=4;

%Hyperparameters for independent Normal-Gamma prior
v0=5;
b0=0*ones(k,1);
b0(2,1)=10;
b0(3,1)=5000;
b0(4,1)=10000;
b0(5,1)=10000;
s02=1/4.0e-8;
capv0=(10000^2)*eye(k);
capv0(2,2)=25;
capv0(3,3)=2500^2;
capv0(4,4)=5000^2;
capv0(5,5)=5000^2;
capv0inv=inv(capv0);

%Ordinary least squares quantities
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
v=n-k;

%Calculate a few quantities outside the loop for later use
v1=v0+n;
v0s02=v0*s02;

%Now start Metropolis-within-Gibbs loop
%candidate generating density is Normal with mean = oldraw
%and variance matrix vscale
%experiment with different values for c and dof
load postvar.out;
%postvar=eye(p);
c=.000001;
c=.1;
vscale= c*postvar;
%store all draws in the following matrices
%initialize them here
b_=[];
h_=[];
a_=[];


%Specify the number of replications
%number of burnin replications
s0=5000;
%number of retained replications
s1=25000;
s=s0+s1;
pswitch=0;
%choose a starting value for h and pdraw
%pdraw is the matrix used for transformation
hdraw=1/s2;
adraw=zeros(p,1);
pcan=zeros(n,n);
pdraw=zeros(n,n);

 
for i = 1:s
   i
  
pdiag=(ones(n,1)+z*adraw).^2;
   for ii = 1:n
    pdraw(ii,ii)=1/sqrt(pdiag(ii,1));
   end
 ystar=pdraw*y;
 xstar=pdraw*x;

    xsquare=xstar'*xstar;
    
    %draw from beta conditional on h
    capv1inv = capv0inv+ hdraw*xsquare;
    capv1=inv(capv1inv);
    b1 = capv1*(capv0inv*b0 + hdraw*xstar'*ystar);
    bdraw=b1 + norm_rnd(capv1);
     
    %draw from h conditional on beta
    s12 = ((ystar-xstar*bdraw)'*(ystar-xstar*bdraw)+v0s02)/v1;
    hdraw=gamm_rnd(1,1,.5*v1,.5*v1*s12);
    
    %Random walk chain M-H algorithm for alpha
     acan=adraw + norm_rnd(vscale);
     
     %Now get acceptance prob for candidate
     pdiag=(ones(n,1)+z*acan).^2;
   for ii = 1:n
        pcan(ii,ii)=1/sqrt(pdiag(ii,1));
   end
 ycan=pcan*y;
 xcan=pcan*x;
 
 %log of conditional posterior for alpha evaluated at acan
 lpostcan =ch13q2w(pdiag,bdraw,hdraw,ycan,xcan,n);
 pdiag=(ones(n,1)+z*adraw).^2;
   for ii = 1:n
    pdraw(ii,ii)=1/sqrt(pdiag(ii,1));
   end
 ystar=pdraw*y;
 xstar=pdraw*x;
 %log of conditional posterior for alpha evaluated at adraw
 lpostdraw =ch13q2w(pdiag,bdraw,hdraw,ystar,xstar,n);
       
%log of acceptance probability
  accprob = exp(lpostcan-lpostdraw);
 % lpostcan
 % lpostdraw
%accept candidate draw with log prob = laccprob, else keep old draw
   if  rand<accprob
       adraw=acan;
       pswitch=pswitch+1;
   end    
    
    if i>s0
        %after discarding burnin, store all draws
        b_ = [b_ bdraw];
        h_ = [h_ hdraw];
        a_ = [a_ adraw];
    end
end


alldraws = [b_' h_' a_'];
result = momentg(alldraws);
means=[result.pmean]';
stdevs=[result.pstd]';
nse=[result.nse1]';


%Print out whatever you want
'Hyperparameters for independent Normal-Gamma prior'
b0
capv0
v0
s02

'Posterior results based on Informative Prior'
'number of burnin replications'
s0
'number of included replications'
s1
'proportion of switches'
pswitch/s
'posterior mean, standard deviation and nse '
'beta followed by h followed by alpha'
[means stdevs nse]

'95% HPDIs'
'beta followed by h followed by alpha'
hpdis=zeros(k+p+1,2);
for ii=1:k+p+1
    hpdis(ii,1:2) = hpdi(alldraws(:,ii),.95);
end
hpdis
avar1=(a_*a_')./s1 - means(k+2:k+p+1,1)*means(k+2:k+p+1,1)';
save postvar.out avar1 -ASCII;
