function plt_varg(results,vnames)
% PURPOSE: Plots Gibbs sampled VAR model results
%---------------------------------------------------
% USAGE:   plt_varg(result,vnames)        
% where:   results = a Gibbs var structure 
%          vnames  = optional vector of variable names
%---------------------------------------------------               
%          e.g. vnames = strvcat('y1','y2','x1');
%---------------------------------------------------                            
% SEE ALSO: plt, prt
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs
if ~isstruct(results)
error('plt_varg requires a structure input');
elseif nargin == 2
nflag = 1;
elseif nargin == 1
nflag = 0;
else
error('Wrong # of inputs to plt_varg');
end;

switch results(1).meth

case{'bvar_g'}

neqs = results(1).neqs;
clf;
for j=1:neqs;
% find predicted and residuals using mean of Gibbs draws
y = results(j).y;
bhat = mean(results(j).bdraw);  % calculate means and std deviations
bhat = bhat';
nvar = results(1).nvar;
nlag = results(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = results(k).y;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if results(1).nx == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat results(1).x ones(nadj,1)];
end;

yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(y,nlag,0) - yhat;
nobs = length(yhat);
yplot = trimr(y,nlag,0);
tt=1:nobs;
subplot(211), plot(tt,yplot,'-',tt,yhat,'--');
legend('Actual','Predicted');
 if nflag == 1
  title(['BVAR Gibbs', ' Actual vs. Predicted  ',vnames(j,:)]);
 else
  title(['BVAR Gibbs', '  Actual vs. Predicted  equation',num2str(j)]);
 end;
subplot(212), plot(tt,resid)
pause;
end; % end of for loop over equations

subplot(111);

case{'rvar_g'}

neqs = results(1).neqs;
clf;
for j=1:neqs;
% find predicted and residuals using mean of Gibbs draws
dy = results(j).dy;
bhat = mean(results(j).bdraw);  % calculate means and std deviations
bhat = bhat';
nvar = results(1).nvar;
nlag = results(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = results(k).dy;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if results(1).nx == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat results(1).x ones(nadj,1)];
end;

yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(dy,nlag,0) - yhat;
nobs = length(yhat);
yplot = trimr(dy,nlag,0);
tt=1:nobs;
subplot(211), plot(tt,yplot,'-',tt,yhat,'--');
 if nflag == 1
  title(['RVAR Gibbs', ' Actual vs. Predicted  ',vnames(j,:)]);
 else
  title(['RVAR Gibbs', '  Actual vs. Predicted  equation',num2str(j)]);
 end;
subplot(212), plot(tt,resid)
pause;
end; % end of for loop over equations

subplot(111);

case {'becm_g','recm_g'} % <=================== becm model

neqs = results(1).neqs;

for j=1:neqs;

dy = results(j).dy;
bhat = mean(results(j).bdraw);  % calculate means and std deviations
bhat = bhat';
nobs = results(1).nobs;
nvar = results(1).nvar;
nlag = results(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = results(k).dy;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if results(1).coint == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat results(1).x ones(nadj,1)];
end;

yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(dy,nlag,0) - yhat;
yplot = trimr(dy,nlag,0);
nobs = length(yhat);
tt=1:nobs;
subplot(211), plot(tt,yplot,'-',tt,yhat,'--');
 if nflag == 1
  if strcmp(results(1).meth,'becm_g')
  title(['BECM Gibbs', ' Actual vs. Predicted  ',vnames(j,:)]);
  else
  title(['RECM Gibbs', ' Actual vs. Predicted  ',vnames(j,:)]);
  end;
 else
  if strcmp(results(1).meth,'becm_g');
  title(['BECM Gibbs', '  Actual vs. Predicted  equation',num2str(j)]);
  else
  title(['RECM Gibbs', '  Actual vs. Predicted  equation',num2str(j)]);
  end;
 end;
subplot(212), plot(tt,resid)
pause;
end; % end of for loop over equations

subplot(111);


otherwise
error('results structure unknown to plt_varg');
end;

  
  
