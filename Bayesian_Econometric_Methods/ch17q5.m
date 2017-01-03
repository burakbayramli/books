%program which does empirical illustration for chapter 17 Exercise 5
%Estimates the ARCH model

load nyse.txt;
%put the data in percentage form to make interpretation a bit easier
y=100*nyse(:,2);
t=size(y,1);
%select order of ARCH model
p=4;
x=ones(t,1);

%starting values
adraw=[1; .1; .1; .1; .1];
bdraw = mean(y);
thetdraw=[adraw; bdraw];
k = size(thetdraw,1);

%functions of interest include number of times MH algorithm accepts a candidate
%and posterior means and variances of alpha and beta
nswitch = 0;
thmean=zeros(k,1);
th2mo=zeros(k,k);

%covariance matrix for increment variable in MH algorithm
%I have set this up so you can either set a particular value for the cov. (useful for getting started)
%or you can read in an estimate of the covariance (useful for final results) 
%comment out as appropriate
sigma=.01*eye(k);
%note ch17q7.out will not exist unless you have already run this program once
%load ch17q7.out;
%sigma=.5*ch17q7;

%number of burning MH replications
nburn=1000;
%number of included MH replications
nrep=100000;
totrep=nrep+nburn;

for irep=1:totrep

   
%log likelihood for the regression model with ARCH errors -- old draw
errs = y - x*bdraw;
vols=adraw(1,1)*ones(t-p,1);
for j=1:p
    vols = vols + adraw(j+1,1)*errs(p+1-j:t-j,1).^2;
end
lterm=(errs(p+1:t,1).^2)./vols;
llike1 = -.5*sum(log(vols)) -.5*sum(lterm);

%take candidate draw
   thetcan = thetdraw + norm_rnd(sigma); 
   acan = thetcan(1:p+1,1);
   bcan = thetcan(p+2:k,1);

%if this draw violates positivity or stationarity restrictions do not accept it
violat =0;
%first check positivity of arch coefficients
for j=1:p+1
    if acan(j,1)<=0
        violat=1;
    end
end
%now check stationarity of arch coefficients
%Matlabs root-finding subroutine orders the polynomial coefficients in the reverse order we have, hence make bstar
    astar = [1];
    for j=2:p+1
        astar = [-acan(j,1) astar];
    end
rr=roots(astar);
%calculate absolute value of roots
rabs=abs(rr);
minrabs=min(rabs);
if minrabs <=1
    violat=1;
end

%if violations of positivity or stationarity, go ahead and calculate acceptance prob

if violat ==0
%log likelihood for the regression model with ARCH errors -- candidate draw
errs = y - x*bcan;
vols=acan(1,1)*ones(t-p,1);
for j=1:p
    vols = vols + acan(j+1,1)*errs(p+1-j:t-j,1).^2;
end
lterm=(errs(p+1:t,1).^2)./vols;
llike2 = -.5*sum(log(vols)) -.5*sum(lterm);

%log of acceptance probability
laccprob = llike2 - llike1;
%now accept candidate with appropriate probability
if laccprob > log(rand)
    thetdraw=thetcan;
    adraw=thetdraw(1:p+1,1);
    bdraw =thetdraw(p+2:k,1);
    nswitch = nswitch+1; 
end

end

%start calculating features of interest after burn-in replications
if irep>nburn
    thmean = thmean + thetdraw;
    th2mo=th2mo + thetdraw*thetdraw';
end
end

thmean = thmean./nrep;
thcov = th2mo./nrep - thmean*thmean';
thsd=zeros(k,1);
for i=1:k
    thsd(i,1)=sqrt(thcov(i,i));
end

'Posterior results for Linear regression model with ARCH errors'
'Characteristics of MH algorithm'
'number of included draws'
nrep
'number of burnin draws'
nburn
'Proportion of candidate draws accepted by MH algorith'
nswitch/totrep
'Characteristics of Posterior'
'Posterior mean and standard deviation of regression coefficients'
[thmean(p+2:k,1) thsd(p+2:k,1)]
'Posterior mean and standard deviation of ARCH coefficients'
[thmean(1:p+1,1) thsd(1:p+1,1)]

%now save posterior covariance matrix for use in MH algorithm
save ch17q7.out thcov -ASCII;

