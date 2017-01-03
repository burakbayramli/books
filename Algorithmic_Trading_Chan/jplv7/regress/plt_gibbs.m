function plt_gibbs(results,vnames)
% PURPOSE: Plots output from Gibbs sampler regression models
%---------------------------------------------------
% USAGE: plt_gibbs(results,vnames)
% Where: results = a structure returned by a Gibbs regression 
%        vnames  = an optional vector of variable names
% --------------------------------------------------
% RETURNS: nothing, just plots regression results
% --------------------------------------------------
% NOTE: user must supply pause commands, none are in plt_gibbs function
%       e.g. plt_gibbs(results);
%            pause;
%            plt_gibbs(results2);
% --------------------------------------------------
% SEE ALSO:  prt, plt
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results); 
   error('plt_reg requires a regression results structure');
end;
nobs = results.nobs;

switch results.meth

case {'ols_g'} % <=================== heteroscedastic linear model
    
% find yhat and residuals

y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(211), plot(tt,results.y,'-',tt,yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(212), plot(tt,resid)
title('Residuals');

case {'ar_g'} % <=================== autoregressive model 


yhat = results.yhat;
[junk ar] = size(results.bdraw);
ar = ar-1;
y = trimr(results.y,ar,0);
resid = y - yhat;

tt=1:length(yhat);
clf;
subplot(211), plot(tt,y,'-',tt,yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(212), plot(tt,resid)
title('Residuals');


case {'bma_g'} % <=================== Bayesian model averaging 

nobs = results.nobs;
nvar = results.nvar;

tt=1:nobs;
clf;
subplot(211), plot(tt,results.y,'-',tt,results.yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(212), plot(tt,results.resid)
title('Residuals');

case {'probit_g'} % <=================== heteroscedastic probit model    
    
y = results.y;
yhat = results.yhat;
resid = y - yhat;
nobs = length(y);

tt=1:nobs;
clf;
subplot(211), plot(tt,y,'-',tt,yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
legend('Actual','Predicted');
subplot(212), plot(tt,resid)
title('Residuals');

case {'tobit_g'} % <=================== heteroscedastic tobit model    
    

y = results.y;
bhat = mean(results.bdraw);  % calculate means and std deviations
bhat = bhat';
nobs = results.nobs;
nvar = results.nvar;
yhat = results.x*bhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(211), plot(tt,y,'-',tt,yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
legend('Actual','Predicted');
subplot(212), plot(tt,resid)
title('Residuals');


otherwise
error('results structure not known by plt_gibbs function');

end;


