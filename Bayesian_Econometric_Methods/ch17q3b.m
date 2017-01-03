%program which does empirical illustration for chapter 17 Exercise 3
%Estimates the homoskedastic threshold autoregressive model with unknown threshold
%and delay parameter
%Threshold trigger can is an exogenous explanatory variable z

load realgdp.out;
rawdat=realgdp;
traw=size(rawdat,1);
yraw=rawdat(:,2);
%Now make growth rate of GDP variable
dyraw = 100*(log(yraw(2:traw,1)) - log(yraw(1:traw-1,1)));
traw=traw-1;
%Specify order of AR
p=4;



%Now lag raw data to set things up for TAR(p) model for GDP growth
%Chop off p initial conditions and make matrices of lagged variables
yun=dyraw(p+1:traw,1);
ylagun=zeros(traw-p,p);
for ii = 1:p
    ylagun(:,ii)= dyraw(p+1-ii:traw-ii,1) ;
end
t=size(yun,1);

%we are going to store results for every possible threshold and delay, initialize them here
lmlikes=[];
threshs=[];
delays=[];
betas=[];
sig2s=[];
%Note you can't average variances across models, so average second moments
b2mos=[];
sig2mos=[];

%do a loop over every possible threshold definition -- that is every value for delay

for id=1:p

%sort the data by the variable used to define the thresholds
%calculate average of threshold variable over last d quarters
ztrig=zeros(t,1);
for isum=1:id
    ztrig =ztrig + ylagun(:,isum);
end
ztrig = ztrig./id;
alldata=[yun ylagun ztrig];
allsort = sortrows(alldata,p+2);
y=allsort(:,1);
ylag=allsort(:,2:p+1);
zsort=allsort(:,p+2);

%now do a big loop over every possible threshold
%make sure at least tprob% of observations lie in each regime
tprob=.15;
ibot=round(tprob*t);
itop=t-ibot;

for i= ibot+1:itop
tau = zsort(i,1);
%construct the dummy variable which = 1 if y(t-1)<=tau

d=zeros(t,1);
for i=1:t
    if zsort(i,1)<=tau
      d(i,1)=1;
  end
end
%Now make up the X matrix
xraw = [ones(t,1) ylag];
k=size(xraw,2);
x1=zeros(t,k); 
x2=zeros(t,k);
for j=1:k
    x1(:,j) = xraw(:,j).*d;
    x2(:,j) = xraw(:,j).*(ones(t,1)-d);
end

x = [x1 x2];
k=size(x,2);

%Now that we have constructed y and X, we just use results 
%for the Normal linear regression model with conjugate prior

%Hyperparameters for natural conjugate prior (chosen fairly arbitrarily)
%We need an informative prior here since marginal likelihoods are being calculated
v0=5;
b0=0*ones(k,1);
s02=1;
capv0=.25*eye(k);
capv0inv=inv(capv0);


%The following, for a given data set and values for prior hyperparameters
%Calculates OLS and posterior quantities

%Ordinary least squares quantities
xsquare=x'*x;
bols = inv(xsquare)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(t-k);
v=t-k;

%Posterior hyperparameters for Normal-Gamma
v1=v0+t;
capv1inv = capv0inv+ xsquare;
capv1=inv(capv1inv);
b1 = capv1*(capv0inv*b0 + xsquare*bols);
v1s12 = v0*s02 + v*s2 + (bols-b0)'*inv(capv0 + inv(xsquare))*(bols-b0);
s12 = v1s12/v1;
bcov = capv1*v1s12/(v1-2);
b2mo = zeros(k,1);
for j=1:k
    b2mo(j,1) = bcov(j,j) + b1(j,1)^2;
end

%posterior mean and variance of error precision
hmean = 1/s12;
hvar=2/(v1s12);
hsd=sqrt(hvar);
%Using standard transformation from Gamma to inverted Gamma,
%get posterior mean and standard deviation for error variance
sigmean=v1s12/(v1-2);
sigvar = (2/(v1-4))*sigmean^2;
sig2mo = sigvar + sigmean^2;

%log of marginal likelihood

    intcon=gammaln(.5*v1) + .5*v0*log(v0*s02)- gammaln(.5*v0) -.5*t*log(pi);
    lmarglik=intcon + .5*log(det(capv1)/det(capv0)) - .5*v1*log(v1s12);
    lmlikes=[lmlikes; lmarglik];
    threshs=[threshs; tau];
    delays = [delays; id];
    betas=[betas; b1'];
    b2mos=[b2mos; b2mo'];
    sig2s=[sig2s; sigmean];
    sig2mos=[sig2mos; sig2mo];
end

end

%Print out whatever you want
'Hyperparameters for informative natural conjugate prior'
b0
capv0
v0
s02


%what we have stored are log marginal likelihoods, transform them to produce probabilities
%next statement to avoid underflow/overflow problems
lmlikes = lmlikes - min(lmlikes);
probs = exp(lmlikes)./sum(exp(lmlikes));

%Now average parameters using probs as weights
bmean=zeros(k,1);
bsd=zeros(k,1);
for i=1:k
    bmean(i,1) = sum(probs.*betas(:,i));
    bsd(i,1) = sqrt(sum(probs.*b2mos(:,i)) - bmean(i,1)^2);
end
'Posterior mean and standard deviation for beta'
[bmean bsd]
sigmean = sum(probs.*sig2s);
sigsd = sqrt(sum(probs.*sig2mos) - sigmean^2);
'Posterior mean and standard deviation for error variance'
[sigmean sigsd]
%[threshs probs]
%For probability for each value of delay parameter
dprob=zeros(p,1);
nthres=itop-ibot;
dlabel=[];
for i=1:p
    dlabel=[dlabel; i];
    dprob(i,1)=sum(probs(nthres*(i-1)+1:i*nthres,1)); 
end
figure(1)
bar(dlabel,dprob)
title('Figure 17.2: Posterior of Delay Parameter')
xlabel('d')
ylabel('Probability')