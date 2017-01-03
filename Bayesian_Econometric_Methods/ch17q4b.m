%program which does empirical illustration for chapter 17 Exercise 4
%Estimates the heteroskedastic threshold autoregressive model with unknown threshold
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
betasa=[];
sig2sa=[];
betasb=[];
sig2sb=[];

%Note you can't average variances across models, so average second moments
b2mosa=[];
sig2mosa=[];
b2mosb=[];
sig2mosb=[];

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
xraw = [ones(t,1) ylag];
k=size(xraw,2);

%now do a big loop over every possible threshold
%make sure at least tprob% of observations lie in each regime
tprob=.15;
ibot=round(tprob*t);
itop=t-ibot;

for i= ibot+1:itop
tau = zsort(i,1);

%construct dependent and explanatory variables in each regime
%since data is ordered by threshold trigger can just take first part as x1, etc.

x1 = xraw(1:i,:);
y1=y(1:i,1);
x2 = xraw(i+1:t,:);
y2=y(i+1:t,1);
t1=size(y1,1);
t2=size(y2,1);


%Now that we have constructed y and X, we just use results 
%for the Normal linear regression model with conjugate prior

%Hyperparameters for natural conjugate prior (chosen fairly arbitrarily)
%We need an informative prior here since marginal likelihoods are being calculated
%Here we are using the same prior for parameters in each regime -- this could easily be relaxed
v0=5;
b0=0*ones(k,1);
s02=1;
capv0=.25*eye(k);
capv0inv=inv(capv0);


%The following, for a given data set and values for prior hyperparameters
%Calculates OLS and posterior quantities
%calculate all these quantities for each regime
%use a and b endings to denote two regimes

%start with first regime
%Ordinary least squares quantities
xsquare=x1'*x1;
bolsa = inv(xsquare)*x1'*y1;
s2a = (y1-x1*bolsa)'*(y1-x1*bolsa)/(t1-k);
va=t1-k;

%Posterior hyperparameters for Normal-Gamma
v1a=v0+t1;
capv1ina = capv0inv+ xsquare;
capv1a=inv(capv1ina);
b1a = capv1a*(capv0inv*b0 + xsquare*bolsa);
v1s12a = v0*s02 + va*s2a + (bolsa-b0)'*inv(capv0 + inv(xsquare))*(bolsa-b0);
s12a = v1s12a/v1a;
bcova = capv1a*v1s12a/(v1a-2);
b2moa = zeros(k,1);
for j=1:k
    b2moa(j,1) = bcova(j,j) + b1a(j,1)^2;
end

%posterior mean and variance of error precision
hmeana = 1/s12a;
hvara=2/(v1s12a);
hsda=sqrt(hvara);
%Using standard transformation from Gamma to inverted Gamma,
%get posterior mean and standard deviation for error variance
sigmeana=v1s12a/(v1a-2);
sigvara = (2/(v1a-4))*sigmeana^2;
sig2moa = sigvara + sigmeana^2;

%log of marginal likelihood in first regime

    intcon=gammaln(.5*v1a) + .5*v0*log(v0*s02)- gammaln(.5*v0) -.5*t1*log(pi);
    lmarglia=intcon + .5*log(det(capv1a)/det(capv0)) - .5*v1a*log(v1s12a);
    
    
    
%now do second regime
%Ordinary least squares quantities
xsquare=x2'*x2;
bolsb = inv(xsquare)*x2'*y2;
s2b = (y2-x2*bolsb)'*(y2-x2*bolsb)/(t2-k);
vb=t2-k;

%Posterior hyperparameters for Normal-Gamma
v1b=v0+t2;
capv1inb = capv0inv+ xsquare;
capv1b=inv(capv1inb);
b1b = capv1b*(capv0inv*b0 + xsquare*bolsb);
v1s12b = v0*s02 + vb*s2b + (bolsb-b0)'*inv(capv0 + inv(xsquare))*(bolsb-b0);
s12b = v1s12b/v1b;
bcovb = capv1b*v1s12b/(v1b-2);
b2mob = zeros(k,1);
for j=1:k
    b2mob(j,1) = bcovb(j,j) + b1b(j,1)^2;
end

%posterior mean and variance of error precision
hmeanb = 1/s12b;
hvarb=2/(v1s12b);
hsdb=sqrt(hvarb);
%Using standard transformation from Gamma to inverted Gamma,
%get posterior mean and standard deviation for error variance
sigmeanb=v1s12b/(v1b-2);
sigvarb = (2/(v1b-4))*sigmeanb^2;
sig2mob = sigvarb + sigmeanb^2;

%log of marginal likelihood in first regime

    intcon=gammaln(.5*v1b) + .5*v0*log(v0*s02)- gammaln(.5*v0) -.5*t2*log(pi);
    lmarglib=intcon + .5*log(det(capv1b)/det(capv0)) - .5*v1b*log(v1s12b);
    
    lmarglik = lmarglia + lmarglib; 
    
    lmlikes=[lmlikes; lmarglik];
    threshs=[threshs; tau];
    delays = [delays; id];
    betasa=[betasa; b1a'];
    b2mosa=[b2mosa; b2moa'];
    sig2sa=[sig2sa; sigmeana];
    sig2mosa=[sig2mosa; sig2moa];
     betasb=[betasb; b1b'];
    b2mosb=[b2mosb; b2mob'];
    sig2sb=[sig2sb; sigmeanb];
    sig2mosb=[sig2mosb; sig2mob];
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
%results for params in first regime
bmeana=zeros(k,1);
bsda=zeros(k,1);
for i=1:k
    bmeana(i,1) = sum(probs.*betasa(:,i));
    bsda(i,1) = sqrt(sum(probs.*b2mosa(:,i)) - bmeana(i,1)^2);
end
'Posterior mean and standard deviation for beta-first regime'
[bmeana bsda]
sigmeana = sum(probs.*sig2sa);
sigsda = sqrt(sum(probs.*sig2mosa) - sigmeana^2);
'Posterior mean and standard deviation for error variance-first regime'
[sigmeana sigsda]
%results for params in second regime
bmeanb=zeros(k,1);
bsdb=zeros(k,1);
for i=1:k
    bmeanb(i,1) = sum(probs.*betasb(:,i));
    bsdb(i,1) = sqrt(sum(probs.*b2mosb(:,i)) - bmeanb(i,1)^2);
end
'Posterior mean and standard deviation for beta-second regime'
[bmeanb bsdb]
sigmeanb = sum(probs.*sig2sb);
sigsdb = sqrt(sum(probs.*sig2mosb) - sigmeanb^2);
'Posterior mean and standard deviation for error variance-second regime'
[sigmeanb sigsdb]
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
title('Figure 17.3: Posterior of Delay Parameter')
xlabel('d')
ylabel('Probability')