function plt_sar(results,vnames)
% PURPOSE: Plots output using SAR model results structures
%---------------------------------------------------
% USAGE: plt_sar(results,vnames) or plt(results,vnames)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%                  e.g. vnames = strvcat('y','constant','x1');
%--------------------------------------------------- 
%  RETURNS: nothing, just plots the spatial regression results
% --------------------------------------------------
% NOTE: called by plt.m
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
 error('plt_sar requires structure argument');
end;

nobs = results.nobs;

switch results.meth

% plot actual vs predicted and residuals
case {'sar'}
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,results.yhat,'--');
title('SAR model  Actual vs. Predicted');
legend('Actual','Predicted');

subplot(2,1,2), plot(tt,results.resid)
title('Residuals');

case {'sar_g','sar_gc'}
y = results.y;
yhat = results.yhat;
resid = y - yhat;

tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if results.novi == 1
 title('SAR homoscedastic MCMC model Actual vs. Predicted');
else
  title('SAR heteroscedastic MCMC model Actual vs. Predicted');
end;
subplot(2,1,2), plot(tt,resid);
title('Residuals');
if results.novi == 0
h2 = figure;
subplot(2,1,1), plot(tt,results.vmean);
title('Mean of Vi draws');
subplot(2,1,2), pltdens(results.pdraw);
title('Posterior Density for rho');    
h3 = figure;
plot(results.bdraw);
title('draws for beta');
xlabel('draws');
ylabel('beta values');
else
h2 = figure;
subplot(2,1,1), pltdens(results.pdraw);
title('Posterior Density for rho');    
subplot(2,1,2), plot(results.bdraw);
title('draws for beta');
xlabel('draws');
ylabel('beta values');
end; 

case {'sarp_g'}
y = results.y;
yprob = results.yprob;
ymean = results.ymean;

tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'o',tt,yprob,'.');
legend('Actual','Predicted');
 title('SAR homoscedastic MCMC probit model Actual vs. Predicted');
subplot(2,1,2), plot(tt,ymean);
title('Mean of y-draws');
h2 = figure;
subplot(2,1,1), pltdens(results.pdraw);
title('Posterior Density for rho');
subplot(2,1,2), plot(results.bdraw);
title('draws for beta');
xlabel('draws');
ylabel('beta values');

case {'sart_g'}
y = results.y;
yhat = results.yhat;
ymean = results.ymean;

tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,yhat,'--');
legend('Actual','Predicted');
if results.novi == 1
 title('SAR homoscedastic MCMC tobit model Actual vs. Predicted');
else
 title('SAR heteroscedastic MCMC tobit model Actual vs. Predicted');
end;
subplot(2,1,2), plot(tt,results.y,'-',tt,ymean,'--');
title('Mean of y-draws');
h2 = figure;
subplot(2,1,1), pltdens(results.pdraw);
title('Posterior Density for rho');
subplot(2,1,2), plot(results.bdraw);
title('draws for beta');
xlabel('draws');
ylabel('beta values');
if results.novi == 0
    h3 = figure;
    plot(results.vmean);
    title('mean of Vi draws');
end;

otherwise
error('method not recognized by plt_sar');
end;





