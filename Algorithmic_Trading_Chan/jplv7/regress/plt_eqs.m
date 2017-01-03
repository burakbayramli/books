function plt_eqs(results,vnames);
% PURPOSE: plots regression actual vs predicted and residuals for:
%          (thsls,sur)
%---------------------------------------------------
% USAGE: plt_eqs(results,vnames);
% % Where: results = a results structure returned by thsls,sur
%          vnames  = an optional vector of variable names
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];       strings
% --------------------------------------------------
% SEE ALSO: plt(), thsls, sur
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs
if ~isstruct(results)
error('plt_eqs requires a structure input');
elseif nargin == 2
nflag = 1;
elseif nargin == 1
nflag = 0;
else
error('Wrong # of inputs to plt_eqs');
end;

nobs = results(1).nobs;
neqs = results(1).neqs;

tt=1:nobs;
clf;
cnt = 1;
for j=1:neqs;
nvar = results(j).nvar;
subplot(2,1,1), plot(tt,results(j).y,'-',tt,results(j).yhat,'--');
legend('Actual','Predicted');
 if nflag == 1
  title([upper(results(1).meth), ' Actual vs. Predicted  ',vnames(cnt,:)]);
 else
  title([upper(results(1).meth), ' Actual vs. Predicted equation ',num2str(j)]);
 end;
subplot(2,1,2), plot(tt,results(j).resid)
cnt = cnt+nvar+1;
pause;
end;

subplot(1,1,1);
