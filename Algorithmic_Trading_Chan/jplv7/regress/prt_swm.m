function prt_swm(results,vnames,fid)
% PURPOSE: Prints output from Switching regression models
%          (switch_em, hmarkov_em)
%---------------------------------------------------
% USAGE: prt_swm(results,vnames,fid)
% Where: results = a structure returned by a switching regression 
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
% NOTES:   e.g. vnames = strvcat('y','x1','x2','cterm')
%           e.g. fid = fopen('ols.out','wr');
%  use prt_swm(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
% RETURNS: nothing, just prints the regression results
% --------------------------------------------------
% SEE ALSO: plt_reg(results), plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results);
 error('prt_swm requires structure argument');
elseif nargin == 1
 nflag = 0; fid = 1;
elseif nargin == 2
 fid = 1; nflag = 1;
elseif nargin == 3
 nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
else
 error('Wrong # of arguments to prt_swm');
end;

method = results(1).meth;

switch method

case{'switch_em','switch_ml'} % <======= EM switching regime regression

nobs = results.nobs;
k1 = results.k1;
k2 = results.k2;
k3 = results.k3;
nvar = results.nvar;

%  make up some variable names
Vnamej = [];
cnt1 = 0; cnt2 = 0; cnt3 = 0;

for i=1:nvar
if i <= k1;
cnt1 = cnt1+1;
    Vnamej{i} = str2mat(['eq1: x',num2str(cnt1)]);
elseif i <= k2;
cnt2 = cnt2+1;
    Vnamej{i} = str2mat(['eq2: x',num2str(cnt2)]);
else
cnt3 = cnt3+1;
    Vnamej{i} = str2mat(['eq3: x',num2str(cnt3)]);
end;
end;

tmp = strvcat(Vnamej);
Vname = [];
for i=1:nvar
   Vname{i} = tmp(i,:);
end;


if (nflag == 1) % the user supplied variable names

[tst_n nsize] = size(vnames);

if tst_n ~= nvar+2
 fprintf(fid,'Wrong # of variable names in prt_swm -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;   
else 
Vname = [];
nmax = min(nsize,16); % truncate vnames to 16-characters

for i=1:k1
Vname{i} = vnames(i+1,1:nmax);
end;

for i=k1+1:k1+k2
Vname{i} = vnames(i+2,1:nmax);
end;

for i=k1+k2+1:k1+k2+k3
Vname{i} = vnames(i+2,1:nmax);
end;

end; % end of if-else

end; % end of nflag issue


fprintf(fid,'\n');
if strcmp(results.meth,'switch_em')
fprintf(fid,'EM Estimates - Switching Regression model \n');
elseif strcmp(results.meth,'switchm_ml')
fprintf(fid,'ML Estimates - Switching Regression model \n');
end;

fprintf(fid,'Regime 1 equation \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr1);
fprintf(fid,'sigma^2        = %9.4f \n',results.sig1);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.k1);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities

% find t-stat marginal probabilities
tstat = zeros(k1,1);
tstat = results.t1;
tout = tdis_prb(tstat,nobs-k1);
tmp = [results.beta1 results.t1 tout];

% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
rnames = 'Variable';
for i=1:k1
   rnames = strvcat(rnames,Vname{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);


fprintf(fid,'\n');
fprintf(fid,'Regime 2 equation \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(k1+2,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr2);
fprintf(fid,'sigma^2        = %9.4f \n',results.sig2);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.k2);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities

% find t-stat marginal probabilities
tstat = zeros(k2,1);
tstat = results.t2;
tout = tdis_prb(tstat,nobs-k2);
tmp = [results.beta2 results.t2 tout];
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
rnames = 'Variable';
for i=k1+1:k1+k2;
   rnames = strvcat(rnames,Vname{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);


fprintf(fid,'Switching equation \n');
if strcmp(results.meth,'switch_em');
fprintf(fid,'Conv criterion = %16.8g \n',results.crit);
end;
fprintf(fid,'# iterations   = %6d    \n',results.iter);
i = find(results.prob1 > 0.5);
n1 = length(i); n2 = nobs - n1;
fprintf(fid,'# obs regime 1 = %6d    \n',n1);
fprintf(fid,'# obs regime 2 = %6d    \n',n2);
fprintf(fid,'log Likelihood = %16.8g  \n',results.like);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.k3);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities

% find t-stat marginal probabilities
tstat = zeros(k3,1);
tstat = results.t3;
tout = tdis_prb(tstat,nobs-k3);
tmp = [results.beta3 results.t3 tout];
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
rnames = 'Variable';
for i=k1+k2+1:k1+k2+k3;
   rnames = strvcat(rnames,Vname{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);


% <=================== end of switch_em  case

case {'hmarkov_em'} %<==== hamilton's markov switching

nobs = results(1).nobs;
nregimes = results(1).regimes;

fprintf(fid,'\n');
if strcmp(method,'hmarkov_em')
fprintf(fid,'Hamilton Markov model EM Estimates \n');
elseif strcmp(method,'hmarkov_ml')
fprintf(fid,'Hamilton Markov model ML Estimates \n');
end;

for jj=1:nregimes
nvar = results(jj).nvar;
%  make up some variable names
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_swm -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 end; % end of if-else
end; % end of nflag issue

if jj == 1
 if (nflag == 1)
 fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
 end;
fprintf(fid,'R-squared         = %9.4f \n',results.rsqr);
fprintf(fid,'R-bar squared     = %9.4f \n',results.rbar);
fprintf(fid,'sigma^2           = %9.4f \n',results.sige);
fprintf(fid,'Log-Likelihood    = %9.4f \n',results.llf);
fprintf(fid,'# of iterations   = %6d   \n',results.iter);
fprintf(fid,'Convg criterion   = %16.8g \n',results.convg);
fprintf(fid,'time in secs      = %9.4f   \n',results.time);
fprintf(fid,'Nobs, Nvars       = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'***************************************\n');

% print van Norden specification test results
if results(1).chisqdwh ~= 0
fprintf(fid,'van Norden Misspecification Test results \n');
 dof = length(results(1).chisqd);
% construct row-labels for test results
int.rnames = 'Specification Test';
int.rnames = strvcat(int.rnames,'Joint Test');
for i= 1:nregimes % intercept terms test results
int.rnames = strvcat(int.rnames,['Regime ',num2str(i), ' AR1-test Constant ']);
end;

% sigma test results
int.rnames = strvcat(int.rnames,'AR1-test for Sigma ');
% transition matrix diagonals
[ntrans junk] = size(results(1).trans);
for i=1:ntrans
int.rnames = strvcat(int.rnames,['AR1-test Trans matrix term ',num2str(i)]);
end;

int.cnames = strvcat('Chi-squared value','marginal prob'); 

mout = [results(1).chisqdwh chis_prb(results(1).chisqdwh,dof)
         results(1).chisqd chis_prb(results(1).chisqd,1)];
mprint(mout,int);

fprintf(fid,'***************************************\n');
end; % end of if results.chisqwh ~= 0
end; % end of if jj=1

fprintf(fid,'Results for Regime = %2d \n',jj);
% fprintf(fid,'Regime R-squared  = %9.4f \n',results.rrsqr(jj));
% fprintf(fid,'# obs in regime   = %6d \n',sum(results.rnobs(:,jj)));
%fprintf(fid,'***************************************\n');



% now print coefficient estimates, t-statistics and probabilities

% find t-stat marginal probabilities
tstat = zeros(nvar,1);
tstat = results(jj).tstat;
tout = tdis_prb(tstat,nobs-nvar);
tmp = [results(jj).beta results(jj).tstat tout];

% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
if jj~= nregimes
fprintf(fid,'***************************************\n');
end;

end; % end of jj-loop over regimes

% <=================== end of hmarkov_em  case

otherwise
error('results structure not known by prt_swm function');

end;


