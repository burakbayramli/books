function plt_far(results,vnames)
% PURPOSE: Plots output using FAR model results structures
%---------------------------------------------------
% USAGE: plt_far(results,vnames) or plt(results,vnames)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%                  e.g. vnames = strvcat('y','constant','x1');
%--------------------------------------------------- 
%  RETURNS: nothing, just plots the spatial regression results
% --------------------------------------------------
% NOTE: this function is called by plt.m
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if ~isstruct(results)
 error('plt_far requires structure argument');
end;

nobs = results.nobs;

switch results.meth

% plot actual vs predicted and residuals
case {'far'}
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,results.yhat,'--');
title('FAR model Actual vs. Predicted');
legend('Actual','Predicted');

subplot(2,1,2), plot(tt,results.resid)
title('Residuals');

case {'far_g','far_gc'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
 title('FAR heteroscedastic MCMC model Actual vs. Predicted');
subplot(2,1,2), plot(tt,resid);
title('Residuals');
h2 = figure;
subplot(2,1,1), plot(tt,results.vmean);
title('Mean of Vi draws');
subplot(2,1,2), pltdens(results.pdraw);
title('Posterior Density for rho');


otherwise
error('method not recognized by plt_far');
end;





