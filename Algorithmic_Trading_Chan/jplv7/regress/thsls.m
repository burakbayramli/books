function result = thsls(neqs,y,Y,X,xall)
% PURPOSE: computes Three-Stage Least-squares Regression
%          for a model with neqs-equations
%---------------------------------------------------
% USAGE: results = thsls(neqs,y,Y,X,xall)
% where: 
%      neqs  = # of equations
%        y   = an 'eq' structure containing dependent variables 
%              e.g. y(1).eq = y1; y(2).eq = y2; y(3).eq = y3;
%        Y   = an 'eq' structure containing RHS endogenous 
%              e.g. Y(1).eq = []; Y(2).eq = [y1 y3]; Y(3).eq = y2;
%        X   = an 'eq' structure containing exogenous/lagged endogenous 
%                 e.g. X(1).eq = [iota x1 x2]; 
%                      X(2).eq = [iota x1]; 
%                      X(3).eq = [iota x1 x2 x3];
% %     xall = matrix of all exogenous variables in the system (defalut
% %            is to construct xall from exogenous variables in X);
% % Richard Just added xall to the argument list to consider the case where
% % exogenous variables enter the system only through identity equations.
%---------------------------------------------------
%        NOTE:  X(i), i=1,...,G should include a constant vector
%               if you want one in the equation
%---------------------------------------------------
% RETURNS a structure:
%     result.meth      = 'thsls'
%     result(eq).beta  = bhat for each equation            
%     result(eq).tstat = tstat for each equation            
%     result(eq).tprob = tprobs for each equation        
%     result(eq).resid = residuals for each equation      
%     result(eq).yhat  = yhats for each equation         
%     result(eq).y     = y for each equation             
%     result(eq).rsqr  = r-squared for each equation     
%     result(eq).rbar  = r-squared adj for each equation  
%     result(eq).nvar  = nvar in each equation     
%     result(eq).sige  = e'e/nobs for each equation 
%     result(eq).dw    = Durbin-Watson       
%     result.nobs      = nobs 
%     result.neqs      = neqs
%     result.sigma     = sig(i,j) across equations
%     result.ccor      = correlation of residuals across equations
% --------------------------------------------------
% SEE ALSO: prt, prt_eqs, plt, plt_eqs
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% Richard Just removed the following statement to allow for specifing xall.
% if (nargin ~=4); error('Wrong # of arguments to thsls'); end;

result.meth = 'thsls';
result.neqs = neqs;

% find the # of equations

chk = fieldnames(y);
if (strcmp(chk,'eq') ~= 1)
error('Use eq as the fieldname for y');
end;

chk = fieldnames(Y);
if (strcmp(chk,'eq') ~= 1)
error('Use eq as the fieldname for Y');
end;

chk = fieldnames(X);
if (strcmp(chk,'eq') ~= 1)
error('Use eq as the fieldname for X');
end;

nobs = length(y(1).eq);

ymat = zeros(nobs,neqs);
yi = zeros(neqs,1);
xi = zeros(neqs,1);

for i=1:neqs;
ymat(:,i) = y(i).eq; % ymat contains dependent variables
[junk yi(i,1)] = size(Y(i).eq); % yi contains # RHS endogenous
[junk xi(i,1)] = size(X(i).eq); % xi contains # RHS exogenous
result(i).nvar = yi(i,1)+xi(i,1);
end;

% % Richard Just added the following two statements to forgo the succeeding
% % 15 statements if xall is passed into the function as a 4th argument.
if nargin == 5
    xinst = xall;
else
% now we have to form xall containing all exogenous in the system
% trick is to find constant term vectors so we end up with only 1 

xall = ones(nobs,1);
for i=1:neqs
 for j=1:xi(i,1);
  % % Richard Just changed the following statement because the test for
  % % X(i).eq(:,j) ~= ones(nobs,1) would fail to treat the two vectors
  % % as ~= when any two corresponding elements were =.  Thus, relevant
  % % exogenous variables such as dummy variables and any variable that
  % % happened to be equal to 1 in at least one observation were excluded.
  if sum((X(i).eq(:,j)-ones(nobs,1)).^2) > 0
  xall = [xall X(i).eq(:,j)];
  end;
   end;
end;

% make it unique

[junk,I,J] = unique(xall','rows');
Is = sort(I);
xinst = xall(:,Is);
end

nrhs = sum(yi)-neqs;
Ymat = zeros(nobs,nrhs);

XX = xinst*inv(xinst'*xinst)*xinst';

Z = zeros(nobs*neqs,sum(xi)+nrhs);
emat = zeros(nobs,neqs);

cnts = 0;

for i=1:neqs;
nexog  = xi(i,1);
nendog = yi(i,1);
Rmat = zeros(nobs,nendog);
Zmat = zeros(nobs,nexog);
 if nexog > 0 % case of exogenous variables
  for j=1:nexog;
  Zmat(:,j) = X(i).eq(:,j);
  end;
 end;
 if nendog > 0 % case of rhs endogenous variables
  for j=1:nendog
  Rmat(:,j) = Y(i).eq(:,j);
  end;
 end;
 % do 2sls and get residuals
 if nendog > 0 & nexog > 0
 res2s = tsls(y(i).eq(:),Rmat,Zmat,xinst);
 emat(:,i) = res2s.resid;
 elseif nendog ==0
 reso = ols(y(i).eq(:),Zmat);
 emat(:,i) = reso.resid; 
 elseif nexog == 0
 error('thsls: no exogenous variables in one equation - not even a constant?');
 end;

 % form matrix [Yi Xi] for this equation
 if nendog > 0 & nexog > 0
 Z((i-1)*nobs+1:(i-1)*nobs+nobs,cnts+1:cnts+nexog+nendog) = [Rmat Zmat];
 elseif nendog > 0 & nexog == 0
 Z((i-1)*nobs+1:(i-1)*nobs+nobs,cnts+1:cnts+nexog+nendog) = [Rmat];
 elseif nendog == 0 & nexog > 0
 Z((i-1)*nobs+1:(i-1)*nobs+nobs,cnts+1:cnts+nexog+nendog) = [Zmat];
 end;
 cnts = cnts + nexog+nendog;
end;

% determine sig(i,j) using of 2sls residuals

sig = (1/nobs)*emat'*emat;

SIGI = kron(inv(sig),XX);

% turn ymat into a stacked vector
yvec = [];
for i=1:neqs;
yvec = [yvec
        y(i).eq];
end;

% compute bhat
[nk1 junk] = size(Z'*SIGI*Z);
xpxi = inv(Z'*SIGI*Z);
bhat = xpxi*(Z'*SIGI*yvec);
nvar   = length(bhat);
yhat = Z*bhat;
resid = yvec - yhat;

for i=1:neqs
result(i).resid = resid((i-1)*nobs+1:i*nobs,1); 
% % Richard Just corrected the following line that had resid'*resid instead
% % of result(i).resid'*result(i).resid.
result(i).sige = (result(i).resid'*result(i).resid)/(nobs);
result(i).yhat = yhat((i-1)*nobs+1:i*nobs,1);
result(i).y = yvec((i-1)*nobs+1:i*nobs,1);
end;

tstat = zeros(nk1,1);

cnt =1;
for i=1:neqs;
nexog  = xi(i,1);
nendog = yi(i,1);
 for j=1:nexog+nendog
 tmp = (xpxi(cnt,cnt));
 tstat(cnt,1) = bhat(cnt,1)/sqrt(tmp);
 cnt = cnt + 1;
 end;
end;

tprob = tdis_prb(tstat,nobs); % use asymptotic tprobs

cnt = 1;
for i=1:neqs;
nvar = result(i).nvar;
result(i).beta  = bhat(cnt:cnt+nvar-1,1);
result(i).tstat = tstat(cnt:cnt+nvar-1,1);
result(i).tprob = tprob(cnt:cnt+nvar-1,1);
yhatg = result(i).yhat;
residg = result(i).resid;
sigu = residg'*residg;
ygm = mean(result(i).y);
yd =  result(i).y - ones(nobs,1)*ygm;
rsqr2 = yd'*yd;
result(i).rsqr = 1 - sigu/rsqr2;
rsqr1 = sigu/(nobs-result(i).nvar);
rsqr2 = rsqr2/(nobs-1.0);
result(i).rbar = 1 - (rsqr1/rsqr2);
ediff = residg(2:nobs) - residg(1:nobs-1);
result(i).dw = diag((ediff'*ediff)./(sigu))'; % durbin-watson
cnt = cnt+nvar;
end;

result(1).nobs = nobs; 
result(1).sigma = sig; % return croos-equation covariances
result(1).ccor = corrcoef(emat); % return cross-equation correlations


