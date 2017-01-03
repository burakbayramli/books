function plt_reg(results,vnames);
% PURPOSE: plots regression actual vs predicted and residuals
%---------------------------------------------------
% USAGE: plt_reg(results);
% where: results is a structure returned by a regression function
%---------------------------------------------------
% RETURNS: nothing, just plots regression results
% --------------------------------------------------
% NOTE: user must supply pause commands, none are in plt_reg function
%       e.g. plt_reg(results);
%            pause;
%            plt_reg(results2);
% --------------------------------------------------
% SEE ALSO: prt_reg(results), prt, plt
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
nobs = results(1).nobs;
method = results(1).meth;

switch method

case {'arma','boxcox','boxcox2','mlogit','logit','ols','olsar1','olsc','probit','ridge', ...
    'theil','tobit','hwhite','tsls','nwest','olsrs'}
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.y,'-',tt,results.yhat,'--');
legend('Actual','Predicted');
title([upper(results.meth), '   Actual vs. Predicted']);
subplot(2,1,2), plot(tt,results.resid)
title('Residuals');

case {'robust','olst','lad'}
tt=1:nobs;
clf;
subplot(311), plot(tt,results.y,'-',tt,results.yhat,'--');
title([upper(results.meth), '   Actual vs. Predicted']);
legend('Actual','Predicted');
subplot(312), plot(tt,results.resid)
title('Residuals');
subplot(313), plot(tt,results.weight)
title('Estimated weights');

case {'switch_em'}
tt=1:nobs;
clf;
plot(tt,results.prob1,'*r',tt,results.prob2,'+k')
title('Estimated probabilities');
legend('regime1','regime2');
pause;
clf;
subplot(2,1,1), plot(tt,results.y,'-r',tt,results.yhat,'--b')
title('Actual vs. Predicted by smoothed probabilities');
legend('actual','predicted');
subplot(2,1,2), plot(tt,results.resid)
title('Residuals');


case {'hmarkov_em'}
tt=1:nobs; tt = tt';
clf;
% find regime 1 y-values based on probs > 0.5
nregimes = results(1).regimes;

for jj=1:nregimes
plot(tt,results(jj).prob,'ob',tt,results(jj).smoothed,'*r',tt,results(jj).pred,'+k');
title(['Probabilities for Regime ', num2str(jj)]);
legend('Unsmoothed','Smoothed','Predicted');
pause;
end;
clf;
subplot(2,1,1), plot(tt,results(1).y,'-r',tt,results(1).yhat,'--b')
title('Actual vs. Predicted by smoothed probabilities');
legend('actual','predicted');
subplot(2,1,2), plot(tt,results(1).resid)
title('Residuals');

otherwise
error('method not recognized by plt_reg');
end;

