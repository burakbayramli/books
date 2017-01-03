function prt_gwr(results,vnames,fid)
% PURPOSE: Prints output from gwr_reg function
%          (Geographically weighted regression)
%---------------------------------------------------
% USAGE: prt_gwr(results,vnames,fid)
% Where: results = a structure returned by gwr_reg
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];
%                 e.g. fid = fopen('gwr.out','wr');
% --------------------------------------------------
% RETURNS:
%        nothing, just prints the regression results
% --------------------------------------------------
% SEE ALSO: gwr_reg()
%---------------------------------------------------   

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin < 1; error('wrong # of arguments to prt_gwr'); end;
if nargin > 3; error('wrong # of arguments to prt_gwr'); end;

nflag = 0;
if nargin == 1; fid = 1;            end;
if nargin == 2; nflag = 1; fid = 1; end;
if nargin == 3; nflag = 1;          end;

nobs = results.nobs;
nvar = results.nvar;
 
%  make up some variable names 
Vname = 'Variable';
for i=1:nvar
    tmp = ['variable  ',num2str(i)];
    Vname = strvcat(Vname,tmp);
end;
if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_gwr -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 Vname = 'Variable'
  for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
  end;
 end; % end of if-else
end; % end of nflag issue

switch results.meth

case {'gwr'} % <=================== gwr regression

fprintf(fid,'\n');
fprintf(fid,'Geometrically weighted regression estimates \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results.rbar);
if strcmp(results.dtype,'exponential') == 1
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
fprintf(fid,'# iterations   = %6d \n',results.iter);
elseif strcmp(results.dtype,'gaussian') == 1
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
fprintf(fid,'# iterations   = %6d \n',results.iter);
else
fprintf(fid,'q-nearest      = %6d \n',results.q);
end;
fprintf(fid,'Decay type     = %12s \n',results.dtype);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities
% find t-stat marginal probabilities
tstat = zeros(nobs,nvar);
tstat = results.tstat;
tout = tdis_prb(tstat,nobs);

% column labels for printing results
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
estring = 'x-coordinate';
nstring = 'y-coordinate';


in.rnames = Vname;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
for i=1:nobs;
east = results.east(i,1); north = results.north(i,1);
sige = results.sige(i,1);
out = [results.beta(i,:)' tstat(i,:)' tout(i,:)'];
fprintf(fid,'Obs = %4d, %8s=%8.4f, %8s=%8.4f, sige=%8.4f \n',i,estring,east,nstring,north,sige);
mprint(out,in);
end;


% <=================== end of gwr case

case {'gwr_logit'} % <=================== gwr_logit regression

fprintf(fid,'\n');
fprintf(fid,'GWR logit estimates \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Estrella R^2   = %9.4f \n',results.rsqr);
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
fprintf(fid,'Log Likelihood = %9.4f \n',results.lik);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0''s, # 1''s = %6d,%6d \n',results.zip,results.one);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities
% find t-stat marginal probabilities
tstat = zeros(nobs,nvar);
tstat = results.tstat;
tout = tdis_prb(tstat,nobs);

% column labels for printing results
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
estring = 'x-coordinate';
nstring = 'y-coordinate';


in.rnames = Vname;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
for i=1:nobs;
east = results.east(i,1); north = results.north(i,1);
sige = results.sige(i,1);
out = [results.beta(i,:)' tstat(i,:)' tout(i,:)'];
fprintf(fid,'Obs = %4d, %8s=%8.4f, %8s=%8.4f, sige=%8.4f \n',i,estring,east,nstring,north,sige);
mprint(out,in);
end;


% <=================== end of gwr_logit case

case {'gwr_probit'} % <=================== gwr_probit regression

fprintf(fid,'\n');
fprintf(fid,'GWR probit estimates \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Estrella R^2   = %9.4f \n',results.rsqr);
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
fprintf(fid,'Log Likelihood = %9.4f \n',results.lik);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0''s, # 1''s = %6d,%6d \n',results.zip,results.one);
fprintf(fid,'***************************************\n');

% now print coefficient estimates, t-statistics and probabilities
% find t-stat marginal probabilities
tstat = zeros(nobs,nvar);
tstat = results.tstat;
tout = tdis_prb(tstat,nobs);

% column labels for printing results
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
estring = 'x-coordinate';
nstring = 'y-coordinate';


in.rnames = Vname;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
for i=1:nobs;
east = results.east(i,1); north = results.north(i,1);
sige = results.sige(i,1);
out = [results.beta(i,:)' tstat(i,:)' tout(i,:)'];
fprintf(fid,'Obs = %4d, %8s=%8.4f, %8s=%8.4f, sige=%8.4f \n',i,estring,east,nstring,north,sige);
mprint(out,in);
end;


% <=================== end of gwr_probit case



case {'bgwr'} % <=================== bgwr regression

nobs = results.nobs;
nvar = results.nvar;
  
% find posterior means
tmp1 = mean(results.bdraw);
bout = squeeze(tmp1);

y = results.y;
yhat = zeros(nobs,1);
for i=1:nobs;
yhat(i,1) =  results.x(i,:)*bout(i,:)';
end;
e = y - yhat; 
tmp1 = std(results.bdraw);
bstd = squeeze(tmp1);
results.tstat = bout./bstd; % trick for printing below
sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian geographically weighted regression model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared        = %9.4f \n',rsqr);
fprintf(fid,'Nobs, Nvars      = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit     = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'r-value          = %9.4f   \n',results.r);
if results.m ~= 0
fprintf(fid,'gam(m,k) r-prior = %6d,%6d \n',results.m,results.k);
end;
fprintf(fid,'delta-value      = %9.4f   \n',results.d);
if results.s ~= 0
fprintf(fid,'gam(m,k) d-prior = %6d,%6d \n',results.s,results.t);
end;
if strcmp(results.dtype,'exponential') == 1
fprintf(fid,'Bandwidth        = %9.4f \n',results.bwidth);
fprintf(fid,'# iterations     = %6d \n',results.iter);
elseif strcmp(results.dtype,'gaussian') == 1
fprintf(fid,'Bandwidth        = %9.4f \n',results.bwidth);
fprintf(fid,'# iterations     = %6d \n',results.iter);
else
fprintf(fid,'q-nearest        = %6d \n',results.q);
end;
fprintf(fid,'Decay type       = %12s \n',results.dtype);
fprintf(fid,'time in secs     = %9.4f   \n',results.time);
fprintf(fid,'prior type       = %10s    \n',results.ptype);
fprintf(fid,'***************************************************************\n');


% now print coefficient estimates, t-statistics and probabilities
% find t-stat marginal probabilities
tstat = zeros(nobs,nvar);
tstat = results.tstat;
tout = tdis_prb(tstat,nobs);

% column labels for printing results
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
estring = 'x-coordinate';
nstring = 'y-coordinate';
sstring = 'sigma';

in.rnames = Vname;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
for i=1:nobs;
east = results.xcoord(i,1); north = results.ycoord(i,1); smean = results.smean(i,1);
out = [bout(i,:)' tstat(i,:)' tout(i,:)'];
fprintf(fid,'Obs = %4d, %8s=%8.4f, %8s=%8.4f %8s=%8.4f \n',i,estring,east,nstring,north,sstring,smean);
mprint(out,in);
end;

case {'bgwrv'} % <=================== bgwrv regression

nobs = results.nobs;
nvar = results.nvar;
  
% find posterior means
tmp1 = mean(results.bdraw);
bout = squeeze(tmp1);

y = results.y;
yhat = zeros(nobs,1);
for i=1:nobs;
yhat(i,1) =  results.x(i,:)*bout(i,:)';
end;
e = y - yhat; 
tmp1 = std(results.bdraw);
bstd = squeeze(tmp1);
results.tstat = bout./bstd; % trick for printing below
sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Heteroscedastic GWR model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared        = %9.4f \n',rsqr);
fprintf(fid,'Nobs, Nvars      = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit     = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'r-value          = %9.4f   \n',results.r);
fprintf(fid,'time in secs     = %9.4f   \n',results.time);
if strcmp(results.dtype,'exponential') == 1
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
elseif strcmp(results.dtype,'gaussian') == 1
fprintf(fid,'Bandwidth      = %9.4f \n',results.bwidth);
else
fprintf(fid,'q-nearest      = %6d \n',results.q);
end;
fprintf(fid,'Decay type     = %12s \n',results.dtype);
fprintf(fid,'***************************************************************\n');


% now print coefficient estimates, t-statistics and probabilities
% find t-stat marginal probabilities
tstat = zeros(nobs,nvar);
tstat = results.tstat;
tout = tdis_prb(tstat,nobs);

% column labels for printing results
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
estring = 'x-coordinate';
nstring = 'y-coordinate';
sstring = 'sigma';

in.rnames = Vname;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
for i=1:nobs;
east = results.xcoord(i,1); north = results.ycoord(i,1);
smean = results.smean(i,1);
out = [bout(i,:)' tstat(i,:)' tout(i,:)'];
fprintf(fid,'Obs = %4d, %8s=%8.4f, %8s=%8.4f, %8s=%8.4f\n',i,estring,east,nstring,north,sstring,smean);
mprint(out,in);
end;


otherwise
error('results structure not known by prt_gwr function');
end;

