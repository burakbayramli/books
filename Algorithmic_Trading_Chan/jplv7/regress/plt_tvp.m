function plt_tvp(results,vnames)
% PURPOSE: Plots output using tvp regression results structures
%---------------------------------------------------
% USAGE: plt_tvp(results,vnames)
% Where: results = a structure returned by a tvp regression 
%        vnames  = an optional vector of variable names
%                  e.g. vnames = strvcat('y','constant','x1');
%--------------------------------------------------- 
%  RETURNS: nothing, just plots the tvp regression results
% --------------------------------------------------
% NOTE: user must supply pause commands, none are in plt_tvp function
%       e.g. plt_tvp(results);
%            pause;
%            plt_tvp(results2);
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('plt_tvp requires structure argument');
end;

nobs = results.nobs;

switch results.meth

case {'tvp'}

n = length(results.y);
beta = results.beta; % an nxk matrix
start = results.start;
y = results.y(start:n,1);
yhat = results.yhat;

e = y - yhat; 
subplot(1,1,1);

 [nobs nvar] = size(beta);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,beta(tt,j));
   title(['TVP coefficient ',num2str(j)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,beta(tt,j));
   title(['TVP coefficient ',vnames(j+1,:)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,y(tt,1),'-',tt,yhat(tt,1),'--');
title('TVP Actual vs. Predicted');
legend('Actual','Predicted');
subplot(2,1,2), plot(tt,e(tt,1))
title('Residuals');
xlabel(['Observations start= ',num2str(start)]);
pause;

% Now plot forecast error variance
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.fvar(tt,1));
title('TVP Conditional variance');
subplot(2,1,2), plot(tt,results.ferror(tt,1))
title('Forecast errors');
xlabel(['Observations start= ',num2str(start)]);
pause;


case {'tvp_garch'}

n = length(results.y);
beta = results.beta; % an nxk matrix
start = results.start;
y = results.y(start:n,1);
yhat = results.yhat;

e = y - yhat; 
subplot(1,1,1);

 [nobs nvar] = size(beta);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,beta(tt,j));
   title(['TVP coefficient ',num2str(j)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,beta(tt,j));
   title(['TVP coefficient ',vnames(j+1,:)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   pause;
   end; % end of j-loop over expanded-variables
  end;

% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,y(tt,1),'-',tt,yhat(tt,1),'--');
title('TVP garch Actual vs. Predicted');
legend('Actual','Predicted');
subplot(2,1,2), plot(tt,e(tt,1))
title('Residuals');
xlabel(['Observations start= ',num2str(start)]);
pause;

% Now plot forecast error variance
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,results.fvar(tt,1));
title('TVP garch Conditional variance');
subplot(2,1,2), plot(tt,results.ferror(tt,1))
title('Forecast errors');
xlabel(['Observations start= ',num2str(start)]);
pause;


% Now plot garch variance
clf;
subplot(1,1,1),
plot(tt,results.sigt(tt,1));
title('Garch sigma');
pause;

case {'tvp_markov'}

prob1 = results.prob1;
prob2 = results.prob2;

beta1 = results.beta1; % an nxk matrix
beta2 = results.beta2;
start = results.start;
n = length(results.y);

y = results.y(start:n,1);
yhat = results.yhat;

e = y - yhat; 
subplot(1,1,1);

 [nobs nvar] = size(beta1);
 tt=1:nobs;
  if nargin < 2 % case of no vnames
   for j=1:nvar;
   plot(tt,beta1(:,j),'-',tt,beta2(:,j),'--');
   title(['TVP Markov state1, state2 coefficients ',num2str(j)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   legend('state1','state2');
   pause;
   end; % end of j-loop over expanded-variables
  else
   for j=1:nvar;
   plot(tt,beta1(:,j),'-',tt,beta2(:,j),'--');
   title(['TVP Markov state1, state2 coefficients ',vnames(j+1,:)]);
   xlabel(['Observations start= ',num2str(start)]);
   ylabel('coefficient');
   legend('state1','state2');
   pause;
   end; % end of j-loop over expanded-variables
  end;


% Now plot probabilities
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,prob1,'-');
title('Probabilities for State 1');
subplot(2,1,2), plot(tt,prob2,'-')
title('Probabilities for State 2');
xlabel(['Observations start= ',num2str(start)]);
pause;



% Now plot actual vs predicted 
tt=1:nobs;
clf;
subplot(2,1,1), plot(tt,y(tt,1),'-',tt,yhat(tt,1),'--');
title('TVP markov  Actual vs. Predicted');
legend('Actual','Predicted');
subplot(2,1,2), plot(tt,e(tt,1))
title('Residuals');
xlabel(['Observations start= ',num2str(start)]);
pause;



otherwise
error('method not recognized by plt_tvp');
end;
