function [data,h]=multigarchsimulate(T,parameters,p,o,q,type,errors,nu2)
% PURPOSE:
%     Simulate the models of multi_garch
% 
% USAGE:
%     [data,h]=multigarchsimulate(T,parameters,p,o,q,type,errors,nu2)
% 
% INPUTS:
%     t: Length of the time series desired
%     parameters: a 1+p+q x 1 vector of inputs where p are ARCH coefs and Q are GARCH coefs
%     P: Positive, scalar integer representing a model order of the ARCH process
%     O: Non-Negative order of the assymetric news impact parameters 
%     Q: Non-Negative scalar integer representing a model order of the GARCH 
%        process: Q is the number of lags of the lagged conditional variances included
%        Can be empty([]) for ARCH process
%    type: One of the types below denoting the type of GARCH
%    errors: Either 'NORMAL', 'STUDENTST', or 'GED', a string
%    nu2: If T or GED are selected the shape parameter
% 
% OUTPUTS:
%     data - T by 1 vector of residuals
%     h - Time-varying variance
% 
% COMMENTS:
%                  Without assymetric terms
%    'GARCH'    -  Normal GARCH Model(see garchpq or fattailed_garch instead) 
%    'AVGARCH'  -  Absolute Value GARCH                   
%    'NGARCH'   -  Non-linear GARCH                       
%    'NAGARCH'  -  Non-Linear Asymetric GARCH             
%                  With assymetric terms
%    'EGARCH'   -  Exponential GARCH                      
%    'TGARCH'   -  Threshold GARCH                        
%    'GJRCARCH' -  GJR Representation of TARCH                    
%    'APGARCH'  -  Asymetric Power GARCH                  
%    'ALLGARCH' -  Asymetric Power GARCH with news impact centering parameter 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

T=T+500;
parameters(find(parameters(1:1+p+q+o) <= 0)) = realmin;
garchparameters=parameters(1:p+q+o+1);
remainingparams=parameters(p+q+2:length(parameters));
constp=garchparameters(1);
archp=garchparameters(2:p+1);
tarchp=garchparameters(p+2:p+o+1);
garchp=garchparameters(p+o+2:p+q+o+1);

if strcmp(type,'GARCH')
    lambda=2;
    nu=2;
    b=0;
elseif strcmp(type,'TGARCH');
    lambda=1;
    nu=1;
    b=0;
elseif strcmp(type,'AVGARCH');
    lambda=1;
    nu=1;
    b=remainingparams(1);
elseif strcmp(type,'NGARCH')
    lambda=remainingparams(1);
    nu=lambda;
    b=0;
elseif strcmp(type,'NAGARCH')
    lambda=2;
    nu=2;
    b=remainingparams(1);
elseif strcmp(type,'APGARCH')
    lambda=remainingparams(1);
    nu=lambda;
    b=0;
elseif strcmp(type,'ALLGARCH')
    lambda=remainingparams(1);
    nu=lambda;
    b=remainingparams(2);
elseif strcmp(type,'GJRGARCH')
    lambda=2;
    nu=2;
    b=0;
else
    error('Do not know hwo to simulate that GARCH')
end

if strcmp(errors,'NORMAL')
    randnum=randn(T,1);    
elseif strcmp(errors,'STUDENTST')
    randnum=ged_rnd(T,nu2);
elseif strcmp(errors,'GED')
    randnum=stdtdis_rnd(T,nu2);
else
    error('Do not know how to simulate the type of errors')
end

UncondStd   = sqrt(constp/(1-sum(archp)-sum(garchp)));
h           = UncondStd*ones(T,1);
m=max([p o q]);

dataneg=zeros(size(h));
dataneg(1:m)=UncondStd/2;
datamb=zeros(size(h));
datamb(1:m)=UncondStd;
data=zeros(size(h));
keyboard
if o>0
   for t = m+1:T
        h(t) = (constp   +  archp'*datamb(t-(1:p)).^nu + tarchp'*dataneg(t-(1:o)).^nu  + garchp'*h(t-(1:q)).^lambda)^(1/lambda);
        data(t)=h(t)*randnum(t);
        datamb(t)=abs(data(t)-b);
        dataneg(t)=abs((data(t)<0)*data(t));
    end
else
    for t = m+1:T
        h(t) = (constp   +  archp'*datamb(t-(1:p)).^nu  + garchp'*h(t-(1:q)).^lambda)^(1/lambda);
        data(t)=h(t)*randnum(t);
        datamb(t)=abs(data(t)-b);
        dataneg(t)=abs((data(t)<0)*data(t));
    end    
end

data=randnum.*h;
data=data(501:T);
h=h(501:T).^2;
