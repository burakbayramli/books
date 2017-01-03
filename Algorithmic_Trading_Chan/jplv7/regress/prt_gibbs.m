function prt_gibbs(results,vnames,fid)
% PURPOSE: Prints output from Gibbs sampler regression models
%---------------------------------------------------
% USAGE: prt_gibbs(results,vnames,fid)
% Where: results = a structure returned by a Gibbs regression 
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
%  NOTES:   e.g. vnames = strvcat('y','cterm','x1','x2');
%           e.g. fid = fopen('ols.out','wr');
%  use prt_gibbs(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the regression results
% --------------------------------------------------
% SEE ALSO: plt, prt, plt_gibbs
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('prt_gibbs requires structure argument');
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
 error('Wrong # of arguments to prt_gibbs');
end;

nobs = results.nobs;
nvar = results.nvar;

if nflag == 1 % user-supplied vnames
[tst_n nsize] = size(vnames);

if strcmp(results.meth,'ar_g') == 0
 for i=1:nvar
 Vname{i} = vnames(i+1,:);
 end;
else % we handle things differently for ar_g model
   Vname{1} = vnames(1,:);
end;

end; % end of if nflag == 1

switch results.meth

case {'ols_g','ols_gf'} % <=================== heteroscedastic linear model
    
    
% we handle these differently depending on the model
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
for i=1:nvar
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;
end;

% check result.pflag for tstat argument
y = results.y;
bhat = mean(results.bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(results.bdraw);
bstd = bstd';

if strcmp(results.pflag,'tstat')
 tstat = bhat./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
else % find plevels
 for i=1:results.nvar;
 if bhat(i,1) > 0
 cnt = find(results.bdraw(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(results.bdraw(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 


smean = mean(results.sdraw);
nobs = results.nobs;
nvar = results.nvar;
yhat = results.yhat;
resid = y - yhat;
sigu = resid'*resid;
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian Heteroscedastic Linear Model Gibbs Estimates \n');
if nflag == 1
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',rbar);
fprintf(fid,'sigma^2        = %9.4f \n',smean);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit   = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs   = %9.4f   \n',results.time);
rmean = mean(results.rdraw);
if rmean ~= 0
fprintf(fid,'rmean          = %9.4f \n',rmean);
else
fprintf(fid,'r-value        = %6d  \n',results.r);
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.pmean results.pstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;
mprint(tmp,pin);

% now print coefficient estimates, t-statistics and probabilities

% column labels for printing results
vstring = 'Variable';
bstring = 'Coefficient';
if strcmp(results.pflag,'tstat') % depends on pflag argument
tstring = 't-statistic';
pstring = 't-probability';
tmp = [bhat tstat tout];
else
tstring = 'Std Deviation';
pstring = 'p-level';
tmp = [bhat bstd tout];
end;

cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%16.6f';
in.fid = fid;
in.cnames = cnames;
in.rnames = rnames;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');
mprint(tmp,in);



case {'ar_g'} % <=================== autoregressive model 

y = results.y;
bhat = mean(results.bdraw);  % calculate means and std deviations
bhat = bhat';
ar = length(bhat)-1;
bstd = std(results.bdraw);
bstd = bstd';
if strcmp(results.pflag,'tstat')
 tstat = bhat./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
else % find plevels
 for i=1:results.nvar;
 if bhat(i,1) > 0
 cnt = find(results.bdraw(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(results.bdraw(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

smean = mean(results.sdraw);
nobs = results.nobs;
nvar = results.nvar;
yhat = results.yhat;
resid = trimr(y,ar,0) - yhat;
sigu = resid'*resid;
ym = trimr(y,ar,0) - ones(nobs-ar,1)*mean(trimr(y,ar,0));
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian Autoregressive Model Gibbs Estimates \n');
if nflag == 1
fprintf(fid,'Dependent Variable = %16s \n',Vname{1});
end;
fprintf(fid,'R-squared      = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',rbar);
fprintf(fid,'sigma^2        = %9.4f \n',smean);
fprintf(fid,'nu,d0          = %6d,%6d \n',results.nu,results.d0);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit   = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'accept rate    = %9.4f\n',results.accept);
fprintf(fid,'time in secs   = %9.4f\n',results.time);
if results.r ~= 0
fprintf(fid,'rvalue         = %9.4f \n',results.r);
end;

if nflag == 0
Vname = [];
% create special variable names for AR(m) model
    Vname{1} = str2mat(['Cons']);
 for i=2:nvar
    Vname{i} = str2mat(['AR ',num2str(i-1)]);
 end;
else
   Vname{1} = 'constant ';
   lnames{1} = '      ';   
 for m=1:ar;
    Vname{m+1} = [vnames(1,:) str2mat([' lag  ',num2str(m)])];
 end;
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
lstring = '        ';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.pmean results.pstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%16.6f';
in.rnames = rnames;
in.cnames = cnames;
in.fid = fid;
mprint(tmp,in); 
fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');
% now print coefficient estimates, t-statistics and probabilities


% column labels for printing results
vstring = 'Variable';
bstring = 'Coefficient';

if strcmp(results.pflag,'tstat') % depends on pflag argument
tstring = 't-statistic';
pstring = 't-probability';
tmp = [bhat tstat tout];
else
tstring = 'Std Deviation';
pstring = 'p-level';
tmp = [bhat bstd tout];
end;


cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%16.6f';
in.fid = fid;
in.cnames = cnames;
in.rnames = rnames;
mprint(tmp,in);

case {'bma_g'} % <=================== Bayesian model averaging 

% we handle these differently depending on the model
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
for i=1:results.nvar
    Vname{i} = str2mat(['v',num2str(i)]);
end;
end;

fprintf(fid,'\n');
fprintf(fid,'Bayesian Model Averaging Estimates \n');
if nflag == 1
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'sigma^2        = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws         = %6d \n',results.ndraw);
fprintf(fid,'nu,lam,phi     = %6.3f,%6.3f,%6.3f \n',results.nu,results.lam,results.phi);
fprintf(fid,'# of models    = %6d   \n',results.nmod);
fprintf(fid,'time(seconds)  = %9.4f \n',results.time);

fprintf(fid,'***************************************************************\n');

outi = find(results.prob > 0.01); % limit printing to models > 1 percent post prob
nmod = length(outi);
out = [results.model(outi,:) results.prob(outi,1)*100 results.visit(outi,1)];
 fmt = '%5d';
 cnames = Vname{1};
 for i=2:results.nvar
 cnames = strvcat(cnames,Vname{i});
 fmt = strvcat(fmt,'%5d');
 end;
 fmt = strvcat(fmt,'%6.3f');   
 fmt = strvcat(fmt,'%5d');   
cnames = strvcat(cnames,'Prob','Visit');

rnames = 'Model';
for i=1:nmod;
    rnames = strvcat(rnames,['model ' num2str(i)]); 
end;

min.cnames = cnames;
min.rnames = rnames;
min.fid = fid;
min.fmt = fmt;
fprintf(fid,'Model averaging information \n');
mprint(out,min);
fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');
tstat = results.tstat;
bhat = results.beta;
nvar = results.nvar+1; % need to include the constant term

% now print coefficient estimates, t-statistics and probabilities

% find t-stat marginal probabilities
tout = tdis_prb(tstat,results.nobs);

% column labels for printing results
vstring = 'Variable';
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';

tmp = [bhat tstat tout];

cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
if nflag == 0
 rnames = strvcat(rnames,'const');
 for i=1:results.nvar
 rnames = strvcat(rnames,Vname{i});
 end;
else
 rnames = strvcat(rnames,'const');
 for i=1:results.nvar
  rnames = strvcat(rnames,Vname{i});
 end;
end;

in2.fmt = '%16.6f';
in2.fid = fid;
in2.rnames = rnames;
in2.cnames = cnames;

mprint(tmp,in2);

case {'probit_g'} % <=================== heteroscedastic probit model    
    
% we handle these differently depending on the model
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
for i=1:nvar
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;
end;

bhat = mean(results.bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(results.bdraw);
bstd = bstd';

if strcmp(results.pflag,'tstat')
 tstat = bhat./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
else % find plevels
 for i=1:results.nvar;
 if bhat(i,1) > 0
 cnt = find(results.bdraw(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(results.bdraw(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

nobs = results.nobs;
nvar = results.nvar;
y = results.y;
zip = length(find(y == 0));
one = nobs - zip;
fprintf(fid,'\n');
fprintf(fid,'Bayesian Heteroscedastic Probit Model Gibbs Estimates \n');
if nflag == 1
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'McFadden R^2    = %9.4f \n',results.r2mf);
fprintf(fid,'Estrella R^2    = %9.4f \n',results.rsqr);
fprintf(fid,'Nobs, Nvars     = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0, 1 y-values = %6d,%6d \n',zip,nobs-zip);
fprintf(fid,'ndraws,nomit    = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs    = %9.4f\n',results.time);
rmean = mean(results.rdraw);
if rmean ~= 0
fprintf(fid,'rmean           = %9.4f \n',rmean);
else
fprintf(fid,'r-value         = %6d  \n',results.r);
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.pmean results.pstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;

mprint(tmp,pin);
fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');
% now print coefficient estimates, t-statistics and probabilities


% column labels for printing results
vstring = 'Variable';
bstring = 'Coefficient';

if strcmp(results.pflag,'tstat') % depends on pflag argument
tstring = 't-statistic';
pstring = 't-probability';
tmp = [bhat tstat tout];
else
tstring = 'Std Deviation';
pstring = 'p-level';
tmp = [bhat bstd tout];
end;


cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%16.6f';
in.fid = fid;
in.cnames = cnames;
in.rnames = rnames;
mprint(tmp,in);

case {'tobit_g'} % <=================== heteroscedastic tobit model    
    
% we handle these differently depending on the model
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
for i=1:nvar
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;
end;

yact = results.y;
bhat = mean(results.bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(results.bdraw);
bstd = bstd';
if strcmp(results.pflag,'tstat')
 tstat = bhat./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
else % find plevels
 for i=1:results.nvar;
 if bhat(i,1) > 0
 cnt = find(results.bdraw(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(results.bdraw(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

nobs = results.nobs;
nvar = results.nvar;
yhat = results.x*bhat;
resid = yact - yhat;
sigu = resid'*resid;
ym = yact - ones(nobs,1)*mean(yact);
rsqr1 = sigu/(nobs-nvar);
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian Heteroscedastic Tobit Model Gibbs Estimates \n');
if nflag == 1
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',rsqr); % based on draws
fprintf(fid,'sigma^2        = %9.4f \n',mean(results.sdraw));
fprintf(fid,'nu,d0          = %6d,%6d \n',results.nu,results.d0);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of censored  = %6d \n',results.nobsc);
fprintf(fid,'ndraws,nomit   = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs   = %9.4f\n',results.time);
rmean = mean(results.rdraw);
if rmean ~= 0
fprintf(fid,'rmean          = %9.4f \n',rmean);
else
fprintf(fid,'r-value        = %6d  \n',results.r);
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.pmean results.pstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;

mprint(tmp,pin);
fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');
% now print coefficient estimates, t-statistics and probabilities

% column labels for printing results
vstring = 'Variable';
bstring = 'Coefficient';
if strcmp(results.pflag,'tstat') % depends on pflag argument
tstring = 't-statistic';
pstring = 't-probability';
tmp = [bhat tstat tout];
else
tstring = 'Std Deviation';
pstring = 'p-level';
tmp = [bhat bstd tout];
end;


cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
in.fmt = '%16.6f';
in.fid = fid;
in.cnames = cnames;
in.rnames = rnames;
mprint(tmp,in);


otherwise
error('results structure not known by prt_gibbs function');

end;


