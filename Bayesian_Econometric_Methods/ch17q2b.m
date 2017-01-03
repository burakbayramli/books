%program which does empirical illustration for chapter 17 Exercise 2
%Estimates the homoskedastic threshold autoregressive model with known threshold

load realgdp.out;
rawdat=realgdp;
traw=size(rawdat,1);
yraw=rawdat(:,2);
%Now make growth rate of GDP variable
dyraw = 100*(log(yraw(2:traw,1)) - log(yraw(1:traw-1,1)));
traw=traw-1;
%Specify order of AR
p=2;



%Now lag raw data to set things up for TAR(p) model for GDP growth
%Chop off p initial conditions and make matrices of lagged variables
y=dyraw(p+1:traw,1);
ylag=zeros(traw-p,p);
for ii = 1:p
    ylag(:,ii)= dyraw(p+1-ii:traw-ii,1) ;
end
t=size(y,1);
%onstruct the dummy variable which = 1 if y(t-1)<=tau
tau=mean(ylag(:,1));
d=zeros(t,1);
for i=1:t
    if ylag(i,1)<=tau
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
%Noninformative prior obtained by setting v0=0 and capv0inv=zeros(k,k);
%v0=0;
%capv0inv=zeros(k,k);
v0=5;
b0=0*ones(k,1);
s02=1;
capv0=eye(k);
capv0inv=inv(capv0);


%The following, for a given data set and values for prior hyperparameters
%Calculates OLS and posterior quantities

%Ordinary least squares quantities
bols = inv(x'*x)*x'*y;
s2 = (y-x*bols)'*(y-x*bols)/(t-k);
bolscov = s2*inv(x'*x);
bolssd=zeros(k,1);
for i = 1:k
bolssd(i,1)=sqrt(bolscov(i,i));
end
v=t-k;

%Posterior hyperparameters for Normal-Gamma
xsquare=x'*x;
v1=v0+t;
capv1inv = capv0inv+ xsquare;
capv1=inv(capv1inv);
b1 = capv1*(capv0inv*b0 + xsquare*bols);
if det(capv0inv)>0
    v1s12 = v0*s02 + v*s2 + (bols-b0)'*inv(capv0 + inv(xsquare))*(bols-b0);
else
    v1s12 = v0*s02 + v*s2;
end
s12 = v1s12/v1;

bcov = capv1*v1s12/(v1-2);
bsd=zeros(k,1);
for i = 1:k
bsd(i,1)=sqrt(bcov(i,i));
end


%posterior mean and variance of error precision
hmean = 1/s12;
hvar=2/(v1s12);
hsd=sqrt(hvar);
%Using standard transformation from Gamma to inverted Gamma,
%get posterior mean and standard deviation for error variance
sigmean=v1s12/(v1-2);
sigvar = (2/(v1-4))*sigmean^2;

%Print out whatever you want
'Hyperparameters for informative natural conjugate prior'
b0
capv0
v0
s02

'Posterior mean and standard deviation for beta'
[b1 bsd]
'Posterior mean and standard deviation for error precision'
[hmean hsd]
'Posterior mean and standard deviation for error variance'
[sigmean sqrt(sigvar)]
