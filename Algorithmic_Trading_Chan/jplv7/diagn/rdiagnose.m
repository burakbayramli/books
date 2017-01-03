function result = rdiagnose(y,x,prt)
% PURPOSE: computes regression diagnostic measures (see RETURNS)
%---------------------------------------------------
% USAGE: result = rdiagnose(y,x,prt)
% where: y = dependent variable vector   (from a regression model)
%        x = independent variable matrix (from a regression model)
%      prt = 1 to print outlier candidates (see NOTES)
%---------------------------------------------------
% RETURNS: a structure:
%        result.meth  = 'diagnostics'
%        result.hatdi = hat-matrix diagonal
%        result.stdr  = standardized residuals
%        result.press = Press residuals
%        result.pstat = Press statistic
%        result.stud  = Studentized residuals
%        result.rstud = R-student residuals
%        result.dffit = dffits
%        result.cookd = Cook's distance
%        result.reid  = OLS residuals
% --------------------------------------------------
% NOTES: if prt = 1, candidate outlier observations where:  
%       result.hatdi > 2*k/n
%       abs(result.rstud) > 2
%       abs(DFBETA) > 2/sqrt(n)
%       abs(DFFIT) > 2*sqrt(k/n)
% are printed along with these statistics  
% --------------------------------------------------
% SEE ALSO: dfbeta, bkw, rdiag
%---------------------------------------------------
% REFERENCES: Belsley, Kuh, Welsch, 1980 Regression Diagnostics,
% Cook and Weisberg 1982, Residuals and Influence in Regression 

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


if nargin == 3
prt = 1;
elseif nargin == 2
prt = 0;
else
error('Wrong # of arguments to diagnose');
end;

result.meth = 'diagnostics';

ores = ols(y,x);
Serr = ores.sige;
XTXI = inv(x'*x);
res  = ores.resid;
result.resid = res;

[n,k]=size(x);

p = rank(x);
h = diag(x*XTXI*x'); % leverage values
iota = ones(n,1);

result.stdr = res ./ Serr;
tmp1 = (n-p)*Serr^2/(n-p-1); tmp2 = (res.^2)/(n-p-1);
SIS   = (tmp1 - tmp2)./(iota-h);
RSTUD = res ./ sqrt(SIS .* (iota-h)); % studentized residuals
PRESS = res ./ (iota - h);
Pstat = PRESS'*PRESS; % press statistic
STRES = res ./ sqrt(Serr^2.*(iota-h)); % std residuals
c = inv(x'*x)*x';

DFBETA = (c'.*kron(RSTUD,ones(1,k)))./sqrt( kron((iota-h),diag(inv(x'*x))'));

DFFIT = ((h./(iota-h)) .^ (1/2)) .* RSTUD;
COOKD = ((STRES.^2 / p) .* h) ./ (iota-h);

result.hatdi = h;
result.cookd = COOKD;
result.rstud = RSTUD;
result.press = PRESS;
result.dfbeta = DFBETA;
result.dffit = DFFIT;
result.pstat = Pstat;
result.stud  = STRES;

if prt == 1

% find outlier candidates
hout = (result.hatdi > 2*k/n);
rout = (abs(result.rstud) > 2);
dfout = (abs(DFFIT) > 2*sqrt(k/n));
hind = find(hout);
rind = find(rout);
dfind = find(dfout);
hmat = h(hout);
rmat = result.rstud(rout);
fmat = result.dffit(dfout);

fprintf('Obs # Hat-Matrix diagonal (leverage points) \n');
for i=1:length(hmat);
fprintf('%4d %16.8f \n',hind(i,1),hmat(i,1));
end;

fprintf('Obs # Studentized residuals \n');
for i=1:length(rmat);
fprintf('%4d %16.8f \n',rind(i,1),rmat(i,1));
end;

fprintf('Obs # DFFITS \n');
for i=1:length(fmat);
fprintf('%4d %16.8f \n',dfind(i,1),fmat(i,1));
end;


for i=1:k
dbout = (abs(DFBETA(:,i) > 2/sqrt(n)));
dbind = find(dbout);
bmat = DFBETA(dbind,i);
 if length(dbind) > 0
 fprintf('Obs DFBETA for # Variable %d \n',i);
  for j=1:length(dbind);
  fprintf('%4d %16.8f \n',dbind(j,1),bmat(j,1));
  end;
 end;
end;


end; % end of if prt == 1
