function results=nwest(y,x,nlag)
% PURPOSE: computes Newey-West adjusted heteroscedastic-serial
%          consistent Least-squares Regression
%---------------------------------------------------
% USAGE: results = nwest(y,x,nlag)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%     nlag = lag length to use
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'newlyw'
%        results.beta  = bhat
%        results.tstat = t-stats
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.dw    = Durbin-Watson Statistic
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector
% --------------------------------------------------
% SEE ALSO: nwest_d, prt(results), plt(results)
%---------------------------------------------------
% References:  Gallant, R. (1987),
%  "Nonlinear Statistical Models," pp.137-139.
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% % jlesage@spatial-econometrics.com



if (nargin ~= 3); error('Wrong # of arguments to nwest'); end;

[nobs nvar] = size(x);

results.meth    = 'nwest';
results.y       = y;
results.nobs    = nobs;
results.nvar    = nvar;

xpxi = inv(x'*x);
results.beta    = xpxi*(x'*y);
results.yhat    = x*results.beta;
results.resid   = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige    = sigu/(nobs-nvar);

% perform Newey-West correction
emat = [];
for i=1:nvar;
emat = [emat
        results.resid'];
end;
       
    hhat=emat.*x';
    G=zeros(nvar,nvar); w=zeros(2*nlag+1,1);
    a=0;

    while a~=nlag+1;
        ga=zeros(nvar,nvar);
        w(nlag+1+a,1)=(nlag+1-a)/(nlag+1);
        za=hhat(:,(a+1):nobs)*hhat(:,1:nobs-a)';
          if a==0;
           ga=ga+za;
          else
           ga=ga+za+za';
          end;
        G=G+w(nlag+1+a,1)*ga;
        a=a+1;
    end; % end of while
    
        V=xpxi*G*xpxi;
        nwerr= sqrt(diag(V));

results.tstat = results.beta./nwerr; % Newey-West t-statistics
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw = diag((ediff'*ediff)./(sigu))'; % durbin-watson

