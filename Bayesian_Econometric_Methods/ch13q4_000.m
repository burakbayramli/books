%program which does empirical illustration for chapter 13 Exercise 4
%AR(p) errors
%Gibbs sampling for independent Normal-Gamma prior for beta and h
%Use Savage-Dickey density ratio to test for AR coefficients equalling 0


load yankees.txt;
t=size(yankees,1);
yraw=yankees(:,2);
xraw=[ones(t,1) yankees(:,5:6) yankees(:,8:8)];
k=size(xraw,2);

%Specify order of AR errors
p=1;
%Chop off p initial conditions and make matrices of lagged variables
x=xraw(p+1:t,:);
y=yraw(p+1:t,1);
ylag=zeros(t-p,p);
xlag=zeros(t-p,k,p);
for ii = 1:p
    ylag(:,ii)= yraw(p+1-ii:t-ii,1);
    xlag(:,:,ii)=xraw(p+1-ii:t-ii,:);
end

%Hyperparameters for independent Normal-Gamma prior for beta and h
v0=0;
b0=0*ones(k,1);
s02=1;
%for noninformative prior comment out following lines and set capv0inv=0
%capv0=(10000^2)*eye(k);
%capv0inv=inv(capv0);
%for noninformative prior use following line
capv0inv=zeros(k,k);

%Hyperparameters for Normal prior for rho
rho0=zeros(p,1);
capvp0=.09*eye(p);
capvp0inv=inv(capvp0);

%Ordinary least squares quantities
bols = inv(xraw'*xraw)*xraw'*yraw;
s2 = (yraw-xraw*bols)'*(yraw-xraw*bols)/(t-k);
v=t-p-k;
bcov=s2*inv(xraw'*xraw);
bsd=zeros(k,1);
for ii = 1:k
    bsd(ii,1)=sqrt(bcov(ii,ii));
end
bt=bols./bsd;

%Calculate a few quantities outside the loop for later use
v1=v0+t-p;
v0s02=v0*s02;
post = zeros(p,1);
%for Savage-Dickey density ratio evaluate prior quantities
prior = zeros(p,1);
for j = 1:p
    prior(j,1) = norm_pdf(0,rho0(j,1),capvp0(j,j));
    %this is for the case with p=1, for p>2 need prior simulation
    intcon = norm_cdf(1,rho0(j,1),capvp0(j,j))-norm_cdf(-1,rho0(j,1),capvp0(j,j));
    prior(j,1)=prior(j,1)/intcon;
end

%store all draws in the following matrices
%initialize them here
b_=[];
h_=[];
rho_=[];
bf_=[];

%Specify the number of replications
%number of burnin replications
s0=10;
%number of retained replications
s1=250;
s=s0+s1;
%choose a starting value for h and pdraw
%pdraw is the matrix used for transformation
hdraw=1/s2;
rhodraw=zeros(p,1);

bige=zeros(t-p,p);
 
for i = 1:s
   i
   ystar = y ;
xstar=x;
   for ii = 1:p
    ystar=ystar - rhodraw(ii,1)*ylag(:,ii);
    xstar=xstar - rhodraw(ii,1)*xlag(:,:,ii);
   end
    xsquare=xstar'*xstar;
   
    %draw from beta conditional on h and rho
    capv1inv = capv0inv+ hdraw*xsquare;
    capv1=inv(capv1inv);
    b1 = capv1*(capv0inv*b0 + hdraw*xstar'*ystar);
    bdraw=b1 + norm_rnd(capv1);
     
    %draw from h conditional on beta and rho
    s12 = ((ystar-xstar*bdraw)'*(ystar-xstar*bdraw)+v0s02)/v1;
    hdraw=gamm_rnd(1,1,.5*v1,.5*v1*s12);
 
  %draw from rho conditional on beta and h
    eraw=yraw-xraw*bdraw;   
    e=eraw(p+1:t,1);
    for ii = 1:p
       bige(:,ii)= eraw(p+1-ii:t-ii,1);
    end
    
    capvp1inv = capvp0inv+ hdraw*bige'*bige;
    capvp1=inv(capvp1inv);
    rho1 = capvp1*(capvp0inv*rho0 + hdraw*bige'*e); 
  %Check and see if draw satisfies stationarity, if not redraw
    statcond=0;
    counter=-1;
    while statcond==0;
        counter=counter+1;
       rhodraw=rho1 + norm_rnd(capvp1);
       %put things in right order for Matlab's rootfinder
       proots=[-flipud(rhodraw); 1];
       r=roots(proots);
       if min(abs(r))>1
         statcond=1;
       end
    end
    %this is for the case with p=1, for p>2 need posterior simulation
 intc = norm_cdf(1,rho1,capvp1)-norm_cdf(-1,rho1,capvp1);
    if i>s0
        %after discarding burnin, store all draws
        b_ = [b_ bdraw];
        h_ = [h_ hdraw];
        rho_ = [rho_ rhodraw];
        %for Savage-Dickey density ratio evaluate posterior quantities
        for j = 1:p
           post(j,1) = norm_pdf(0,rho1(j,1),capvp1(j,j));
           post(j,1)=post(j,1)/intc;
        end
        bfdraw = post./prior;
        bf_ = [bf_ bfdraw];
       
    end
end


alldraws = [b_' h_' rho_'];
result = momentg(alldraws);
means=[result.pmean]';
stdevs=[result.pstd]';
nse=[result.nse1]';


%Print out whatever you want
'Hyperparameters for independent Normal-Gamma prior for b and h'
b0
capv0inv
v0
s02
'Prior for rho is Normal with Mean and variance'
rho0
capvp0
'Ols Estimates and t-stats'
[bols bt]


'Posterior results'
'number of burnin replications'
s0
'number of included replications'
s1

'posterior mean, standard deviation and nse '
'beta followed by h followed by rho'
[means stdevs nse]

'95% HPDIs'
'beta followed by h followed by alpha'
hpdis=zeros(k+p+1,2);
for ii=1:k+p+1
    hpdis(ii,1:2) = hpdi(alldraws(:,ii),.95);
end
hpdis

'Bayes factor for testing rho(i)=0 for i=1,..,p'
bfmean = mean(bf_')';
bfmean