%program which does empirical illustration for Question 3 of chapter 13
%Heteroskedasticity of unknown form = independent Student-t error model
%Gibbs sampling for independent Normal-Gamma prior for beta and h
%load in the data set. Here use house price data from hprice.txt

load hprice.txt;
n=size(hprice,1);
y=hprice(:,1);
x=hprice(:,2:5);
x=[ones(n,1) x];
k=5;

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
%Prior for degrees of freedom is exponential with mean vl0
vl0=25;

%Ordinary least squares quantities
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(n-k);
v=n-k;

%Calculate a few quantities outside the loop for later use
v1=v0+n;
v0s02=v0*s02;

%Now start things for Gibbs loop
%candidate generating density for dof is Normal with mean = oldraw
%and variance vscale
%experiment with different values for c 
%load postvar.out;
postvar=1;
c=.25;
vscale= c*postvar;
%store all draws in the following matrices
%initialize them here
b_=[];
h_=[];
vl_=[];
pswitch=0;


%Specify the number of replications
%number of burnin replications
s0=25000;
%number of retained replications
s1=5000;
s=s0+s1;
pswitch=0;
%choose a starting value for h and pdraw
%pdraw is the matrix used for transformation
hdraw=1/s2;
lamdraw=ones(n,1);
pdraw=zeros(n,n);
vldraw=20;
   for ii = 1:n
    pdraw(ii,ii)=sqrt(lamdraw(ii,1));
   end
 
for i = 1:s
   i
    ystar=pdraw*y;
    xstar=pdraw*x;
    xsquare=xstar'*xstar;
    
    %draw from beta conditional on rest
    capv1inv = capv0inv+ hdraw*xsquare;
    capv1=inv(capv1inv);
    b1 = capv1*(capv0inv*b0 + hdraw*xstar'*ystar);
    bdraw=b1 + norm_rnd(capv1);
     
    %draw from h conditional on rest
    s12 = ((ystar-xstar*bdraw)'*(ystar-xstar*bdraw)+v0s02)/v1;
    hdraw=gamm_rnd(1,1,.5*v1,.5*v1*s12);

    
    %Random walk Metropolis step for dof
    temp=-log(lamdraw) + lamdraw;
    nu=1/vl0 + .5*sum(temp);
    
     vlcan=vldraw+norm_rnd(vscale); 
     if vlcan>0
        lpostcan = .5*n*vlcan*log(.5*vlcan) -n*gammaln(.5*vlcan)...
        -nu*vlcan;
        lpostdraw = .5*n*vldraw*log(.5*vldraw) -n*gammaln(.5*vldraw)...
        -nu*vldraw;
        accprob = exp(lpostcan-lpostdraw);
        accprob
     else
        accprob=0;
     end
%accept candidate draw with log prob = laccprob, else keep old draw
   if  rand<accprob
       vldraw=vlcan;
       pswitch=pswitch+1;
   end    
     
   
    %Draw from lamda conditional on rest
    errors=y-x*bdraw;
    dof=vldraw+1;
    for ii = 1:n
       temp=(hdraw*errors(ii,1)^2 + vldraw);
       lamdraw(ii,1)=gamm_rnd(1,1,.5*dof,.5*temp);
    pdraw(ii,ii)=sqrt(lamdraw(ii,1));
    end
    
    if i>s0
        %after discarding burnin, store all draws
        b_ = [b_ bdraw];
        h_ = [h_ hdraw];
        vl_ = [vl_ vldraw];
    end
end


alldraws = [b_' h_' vl_'];
result = momentg(alldraws);
means=[result.pmean]';
stdevs=[result.pstd]';
nse=[result.nse1]';


%Print out whatever you want
'Hyperparameters for independent Normal-Gamma prior for b and h'
b0
capv0
v0
s02
'Prior mean for exponential prior for dof'
vl0

'Posterior results based on Informative Prior'
'number of burnin replications'
s0
'number of included replications'
s1
'proportion of switches in Metropolis step'
pswitch/s
'posterior mean, standard deviation and nse '
'beta followed by h followed by dof'
[means stdevs nse]

'95% HPDIs'
'beta followed by h followed by alpha'
hpdis=zeros(k+2,2);
for ii=1:k+2
    hpdis(ii,1:2) = hpdi(alldraws(:,ii),.95);
end
hpdis
vvar1=(vl_*vl_')./s1 - means(k+2,1)*means(k+2,1)';
save postvar.out vvar1 -ASCII;



hist(vl_',20)
title('Figure 13.1: Posterior Density for Degrees of Freedom')
xlabel('Degrees of Freedom')