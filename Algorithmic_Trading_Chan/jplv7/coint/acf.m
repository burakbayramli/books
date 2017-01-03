function results =  acf(x,k,p)
% PURPOSE: Estimate the coefficients of the autocorrelation 
%          (covariance with its own lagged value)
%          function up to a particular lag {Xk = Cov[Yt,Yt-k]}
%--------------------------------------------------------------
% USAGE: results = acf(x,k)
% where:      x = a time-series vector
%             k = maximun lag considered 
%             p = Plot autocorrelations
%                 p =  1, Plot function 
%--------------------------------------------------------------
% RETURNS: a results structure
%         results.meth  = 'acf'
%         results.ac    = autocorrelation coefficients   
%         results.k     = dimension of the lag considered
%         results.lowb  = lower bound for 95% confidence interval
%         results.Topb  = Top bound for 95% confidence interval
%         results.qstat = Ljung-Box Q Statistic
%         results.prob  = area under de chi^(2) distribution
%                         used in testing the existance of 
%                         autocorrelation for the kth lag. 
%
%--------------------------------------------------------------
% SEE ALSO: 
%--------------------------------------------------------------
% References: 
% * Judge G., Hill C., Griffiths W., Lütkepohl H., Tsoung-Chao L,
%   Introduction to the Theory and Practice of Econometrics, 
%   1988, pg. 681-690.
% * Hill C. Computer Handbook to Accompany Introduction to the Theory 
%   and Practice of Econometrics,1988, pg. 158-159.

% error checking on inputs
results.meth= 'acf';

if (nargin > 3)
   error('Wrong # of arguments to acf');
elseif(nargin == 3)
   p = 1;
elseif(nargin == 2) 
   p = 0;
else
   k = round(rows(x)/4);
end;
 
if  (cols(x) > 1)
 error('acf cannot handle a matrix -- only vectors');
end;

nobs = rows(x);
results.k = k; 
%center data
xc= x-mean(x);
%variance
v0= (xc'*xc)/nobs; %not v0 = cov(x) see reference 

for i=1:k
   yt= xc(1:nobs-i,1);
   ytk= xc(1+i:nobs,1);
   ck = (yt'*ytk)/nobs;
   xk(i,1)=ck/v0;
   %ckbar = (yt'*ytk)/(nobs-k); 
   %xkbar(k,1)=ck/v0; %asymptotically equivalent to xk
end

results.ac=xk;

%Significance of xk (95% confidence intervals)
   results.lowb= -2/sqrt(nobs)*ones(k,1);
   results.topb= 2/sqrt(nobs)*ones(k,1);
   
%Diagnostics (Ljung & Box, 1978) Q-Statistic
qstat=zeros(k+1,1);
for j=2:k+1
   qstat(j,1) = (xk(j-1,1)^2)/(nobs-(j-1))+ qstat(j-1,1);
   prob(j,1)=  1-chis_prb(abs((nobs*(nobs+2))*qstat(j,1)),j);
end

qstat=trimr((nobs*(nobs+2))*qstat,1,0);
prob =trimr(prob,1,0);
results.qstat=qstat;
results.prob=prob;


