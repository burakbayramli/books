function plt_var(results,vnames);
% PURPOSE: plots VAR model actual vs predicted and residuals
%---------------------------------------------------
% USAGE: plt_var(results,vnames);
% where:  results  = a vare structure 
%          vnames  = an optional vector of variable names
%---------------------------------------------------               
%          e.g. vnames = strvcat('y1','y2','x1');
% --------------------------------------------------
% SEE ALSO: plt, prt, prt_var
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs
if ~isstruct(results)
error('plt_var requires a structure input');
elseif nargin == 2
nflag = 1;
elseif nargin == 1
nflag = 0;
else
error('Wrong # of inputs to plt_var');
end;

nobs = results(1).nobs;
neqs = results(1).neqs;
tt=1:nobs;
clf;
for j=1:neqs;
subplot(211), plot(tt,results(j).y,'-',tt,results(j).yhat,'--');
legend('Actual','Predicted');
 if nflag == 1
  title([upper(results(1).meth), ' Actual vs. Predicted  ',vnames(j,:)]);
 else
  title([upper(results(1).meth), '  Actual vs. Predicted  equation',num2str(j)]);
 end;
subplot(212), plot(tt,results(j).resid)
pause;
end;

subplot(111);
