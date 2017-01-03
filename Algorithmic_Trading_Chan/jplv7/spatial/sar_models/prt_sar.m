function prt_sar(results,vnames,fid)
% PURPOSE: Prints output using SAR results structures
%---------------------------------------------------
% USAGE: prt_sar(results,vnames,fid)
% Where: results = a structure returned by a SAR model
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_spat(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the SAR results
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

if ~isstruct(results)
 error('prt_sar requires structure argument');
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
 error('Wrong # of arguments to prt_sar');
end;


nvar = results.nvar;
nobs = results.nobs;
cflag = results.cflag;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_sdm -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 end
end;

% handling of vnames
Vname = 'Variable';
if nflag == 0 % no user-supplied vnames or an incorrect vnames argument
    if cflag == 1 % a constant term

        Vname = strvcat(Vname,'constant');
     for i=1:nvar-1
        tmp = ['variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
 
    elseif cflag == 0 % no constant term

     for i=1:nvar
        tmp = ['variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
    end;
 
     
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');

elseif (nflag == 1) % the user supplied variable names
    if cflag == 0 % no constant term
    Vname = 'Variable';
     for i=1:nvar
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
     elseif cflag == 1 % a constant term
     Vname = 'Variable';
     for i=1:nvar
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
    end; % end of cflag issue       
 
end; % end of nflag issue



switch results.meth

case {'sar'} % <=================== max lik spatial autoregressive model

nobs = results.nobs;
nvar = results.nvar;
ndraw = results.ndraw;

% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;

% Compute means, std deviation and upper and lower 0.99 intervals
iter = ndraw;
p = results.p;
total_out = zeros(p,5);
total_save = zeros(ndraw,p);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
total_mean = mean(tmp);
total_std = std(tmp);
% Bayesian 0.99 credible intervals
% for the cumulative total effects
total_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
total_save(:,i) = total_sum;
bounds = cr_interval(total_sum,0.99);
cmean = mean(total_sum);
smean = std(total_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
total_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
end;

% now do indirect effects
indirect_out = zeros(p,5);
indirect_save = zeros(ndraw,p);
for i=1:p;
tmp = squeeze(indirect(:,i,:)); % an ndraw by 1 by ntraces matrix
indirect_mean = mean(tmp);
indirect_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative indirect effects
indirect_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
indirect_save(:,i) = indirect_sum;
bounds = cr_interval(indirect_sum,0.99);
cmean = mean(indirect_sum);
smean = std(indirect_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
indirect_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds  ];
end;


% now do direct effects
direct_out = zeros(p,5);
direct_save = zeros(ndraw,p);
for i=1:p;
tmp = squeeze(direct(:,i,:)); % an ndraw by 1 by ntraces matrix
direct_mean = mean(tmp);
direct_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative direct effects
direct_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
direct_save(:,i) = direct_sum;
bounds = cr_interval(direct_sum,0.99);
cmean = mean(direct_sum);
smean = std(direct_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds  ];
end;


fprintf(fid,'\n');
fprintf(fid,'Spatial autoregressive Model Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'log-likelihood     = %16.8g \n',results.lik);
fprintf(fid,'# of iterations    = %6d   \n',results.iter);
fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.time1 ~= 0
fprintf(fid,'time for lndet     = %9.4f \n',results.time1);
end;
if results.time2 ~= 0
fprintf(fid,'time for eigs      = %9.4f \n',results.time2);
end;
if results.time3 ~= 0
fprintf(fid,'time for t-stats   = %9.4f \n',results.time3);
end;
if results.time5 ~= 0
fprintf(fid,'time for x-impacts = %9.4f \n',results.time5);
fprintf(fid,'# draws  x-impacts = %9d   \n',results.ndraw);
end;    

if results.lflag == 0
fprintf(fid,'No lndet approximation used \n');
end;
% put in information regarding Pace and Barry approximations
if results.lflag == 1
fprintf(fid,'Pace and Barry, 1999 MC lndet approximation used \n');
fprintf(fid,'order for MC appr  = %6d  \n',results.order);
fprintf(fid,'iter  for MC appr  = %6d  \n',results.miter);
end;
if results.lflag == 2
fprintf(fid,'Pace and Barry, 1998 spline lndet approximation used \n');
end;

fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho];
    
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% now print x-effects estimates

bstring = 'Coefficient'; 
tstring = 't-stat'; 
pstring = 't-prob';
lstring = 'lower 01';
ustring = 'upper 99';
cnames = strvcat(bstring,tstring,pstring,lstring,ustring);
ini.cnames = cnames;
ini.width = 2000;

% print effects estimates
if cflag == 1
vnameso = strvcat(Vname(3:end-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-1,:));    
end
ini.rnames = strvcat('Direct  ',vnameso);
ini.fmt = '%16.6f';
ini.fid = fid;

% set up print out matrix
printout = direct_out;
mprint(printout,ini);

printout = indirect_out;
ini.rnames = strvcat('Indirect',vnameso);
mprint(printout,ini);

printout = total_out;
ini.rnames = strvcat('Total   ',vnameso);
mprint(printout,ini);




        
% <=================== end of sar case

case {'sar_g'} % <=================== MCMC spatial autoregressive model


nobs = results.nobs;
nvar = results.nvar;
ndraw = results.ndraw;
nomit = results.nomit;

% extract posterior means
bout = [results.beta
        results.rho];
sige = results.sige;
    tmp1 = std(results.bdraw);
    tmp2 = std(results.pdraw);
    bstd = [tmp1'
            tmp2];  

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 for i=1:results.nvar+1;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

rsqr = results.rsqr;

% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;

% Compute means, std deviation and upper and lower 0.99 intervals
iter = ndraw-nomit;
p = results.p;
total_out = zeros(p,5);
total_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
total_mean = mean(tmp);
total_std = std(tmp);
% Bayesian 0.99 credible intervals
% for the cumulative total effects
total_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
total_save(:,i) = total_sum;
bounds = cr_interval(total_sum,0.99);
bounds2 = cr_interval(total_sum,0.95);
cmean = mean(total_sum);
smean = std(total_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
total_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
total_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;

% now do indirect effects
indirect_out = zeros(p,5);
indirect_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(indirect(:,i,:)); % an ndraw by 1 by ntraces matrix
indirect_mean = mean(tmp);
indirect_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative indirect effects
indirect_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
indirect_save(:,i) = indirect_sum;
bounds = cr_interval(indirect_sum,0.99);
bounds2 = cr_interval(indirect_sum,0.95);
cmean = mean(indirect_sum);
smean = std(indirect_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
indirect_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
indirect_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];    
end;
end;


% now do direct effects
direct_out = zeros(p,5);
direct_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(direct(:,i,:)); % an ndraw by 1 by ntraces matrix
direct_mean = mean(tmp);
direct_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative direct effects
direct_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
direct_save(:,i) = direct_sum;
bounds = cr_interval(direct_sum,0.99);
bounds2 = cr_interval(direct_sum,0.95);
cmean = mean(direct_sum);
smean = std(direct_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
direct_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];    
end;
end;


fprintf(fid,'\n');
fprintf(fid,'Bayesian spatial autoregressive model \n');
if results.novi == 1
    fprintf(fid,'Homoscedastic version \n');
elseif results.novi == 0
    fprintf(fid,'Heteroscedastic model \n');
end;
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared       = %9.4f \n',results.rbar);
fprintf(fid,'mean of sige draws = %9.4f \n',results.sige);
fprintf(fid,'sige, epe/(n-k)    = %9.4f \n',results.sigma);
if (results.rdraw == 0 & results.novi == 0)
fprintf(fid,'r-value            = %6d   \n',results.r);
elseif (results.rdraw ~= 0  & results.novi == 0)
fprintf(fid,'mean of rdraws     = %9.4f \n',mean(results.rdraw));
fprintf(fid,'gam(m,k) prior     = %6d,%6d \n',results.m,results.k);
end;  
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'total time in secs = %9.4f   \n',results.time);
if results.time1 ~= 0
fprintf(fid,'time for eigs      = %9.4f \n',results.time1);
end;
if results.time2 ~= 0
fprintf(fid,'time for lndet     = %9.4f \n',results.time2);
end;
if results.time3 ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.time3);
end;

if results.lflag == 0
fprintf(fid,'No lndet approximation used \n');
end;
% put in information regarding Pace and Barry approximations
if results.lflag == 1
fprintf(fid,'Pace and Barry, 1999 MC lndet approximation used \n');
fprintf(fid,'order for MC appr  = %6d  \n',results.order);
fprintf(fid,'iter  for MC appr  = %6d  \n',results.iter);
end;
if results.lflag == 2
fprintf(fid,'Pace and Barry, 1998 spline lndet approximation used \n');
end;

fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'***************************************************************\n');


if (results.priorb == 1)
    % non-diffuse prior, so print it
vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bmean results.bstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname(i+1,:));
end;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;

mprint(tmp,pin);
fprintf(fid,'***************************************************************\n');
end;
fprintf(fid,'      Posterior Estimates \n');

 if strcmp(results.tflag,'tstat')
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities
      
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
 else % use p-levels for Bayesian results
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Std Deviation'; pstring = 'p-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;

% now print x-effects estimates

if strcmp(results.tflag,'tstat')
bstring = 'Coefficient'; 
tstring = 't-stat'; 
pstring = 't-prob';
lstring = 'lower 01';
ustring = 'upper 99';
cnames = strvcat(bstring,tstring,pstring,lstring,ustring);
else
bstring = 'Coefficient'; 
lstring = 'lower 01';
ustring = 'upper 99';
lstring2 = 'lower 05';
ustring2 = 'upper 95';
cnames = strvcat(lstring, lstring2, bstring, ustring2, ustring);
end;

ini.width = 2000;
ini.cnames = cnames;

% print effects estimates

if cflag == 1
vnameso = strvcat(Vname(3:end-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-1,:));
end    
ini.rnames = strvcat('Direct',vnameso);
ini.fmt = '%16.6f';
ini.fid = fid;

% set up print out matrix
printout = direct_out;
mprint(printout,ini);

printout = indirect_out;
ini.rnames = strvcat('Indirect',vnameso);
mprint(printout,ini);

printout = total_out;
ini.rnames = strvcat('Total',vnameso);
mprint(printout,ini);


return;

% <=================== end of sar_g case

case {'sar_gv'} % <=================== MCMC spatial autoregressive model
                % that produces r-value posterior based on draws


nobs = results.nobs;
nvar = results.nvar;
ndraw = results.ndraw;
nomit = results.nomit;


% extract posterior means
bout = [results.beta
        results.rho];
sige = results.sige;
    tmp1 = std(results.bdraw);
    tmp2 = std(results.pdraw);
    bstd = [tmp1'
            tmp2];  

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 for i=1:results.nvar+1;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;

% Compute means, std deviation and upper and lower 0.99 intervals
iter = ndraw-nomit;
p = results.p;
total_out = zeros(p,5);
total_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
total_mean = mean(tmp);
total_std = std(tmp);
% Bayesian 0.99 credible intervals
% for the cumulative total effects
total_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
total_save(:,i) = total_sum;
bounds = cr_interval(total_sum,0.99);
bounds2 = cr_interval(total_sum,0.95);
cmean = mean(total_sum);
smean = std(total_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
total_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
total_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;

% now do indirect effects
indirect_out = zeros(p,5);
indirect_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(indirect(:,i,:)); % an ndraw by 1 by ntraces matrix
indirect_mean = mean(tmp);
indirect_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative indirect effects
indirect_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
indirect_save(:,i) = indirect_sum;
bounds = cr_interval(indirect_sum,0.99);
bounds2 = cr_interval(indirect_sum,0.95);
cmean = mean(indirect_sum);
smean = std(indirect_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
indirect_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
indirect_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];    
end;
end;


% now do direct effects
direct_out = zeros(p,5);
direct_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(direct(:,i,:)); % an ndraw by 1 by ntraces matrix
direct_mean = mean(tmp);
direct_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative direct effects
direct_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
direct_save(:,i) = direct_sum;
bounds = cr_interval(direct_sum,0.99);
bounds2 = cr_interval(direct_sum,0.95);
cmean = mean(direct_sum);
smean = std(direct_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
direct_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];    
end;
end;


rsqr = results.rsqr;

fprintf(fid,'\n');
fprintf(fid,'Bayesian spatial autoregressive model \n');
    fprintf(fid,'Heteroscedastic version with r-value estimate \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared       = %9.4f \n',results.rbar);
fprintf(fid,'mean of sige draws = %9.4f \n',results.sige);
fprintf(fid,'sige, epe/(n-k)    = %9.4f \n',results.sigma);
fprintf(fid,'mean r-value       = %9.4f \n',mean(results.rdraw));
fprintf(fid,'std  r-value       = %9.4f \n',std(results.rdraw));
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'total time in secs = %9.4f   \n',results.time);
if results.time1 ~= 0
fprintf(fid,'time for eigs      = %9.4f \n',results.time1);
end;
if results.time2 ~= 0
fprintf(fid,'time for lndet     = %9.4f \n',results.time2);
end;
if results.time3 ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.time3);
end;

if results.lflag == 0
fprintf(fid,'No lndet approximation used \n');
end;
% put in information regarding Pace and Barry approximations
if results.lflag == 1
fprintf(fid,'Pace and Barry, 1999 MC lndet approximation used \n');
fprintf(fid,'order for MC appr  = %6d  \n',results.order);
fprintf(fid,'iter  for MC appr  = %6d  \n',results.iter);
end;
if results.lflag == 2
fprintf(fid,'Pace and Barry, 1998 spline lndet approximation used \n');
end;

fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'***************************************************************\n');


if (results.priorb == 1)
    % non-diffuse prior, so print it
vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bmean results.bstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname(i+1,:));
end;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;

mprint(tmp,pin);
fprintf(fid,'***************************************************************\n');
end;
fprintf(fid,'      Posterior Estimates \n');

 if strcmp(results.tflag,'tstat')
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities
      
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
 else % use p-levels for Bayesian results
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Std Deviation'; pstring = 'p-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;

% now print x-effects estimates

if strcmp(results.tflag,'tstat')
bstring = 'Coefficient'; 
tstring = 't-stat'; 
pstring = 't-prob';
lstring = 'lower 01';
ustring = 'upper 99';
cnames = strvcat(bstring,tstring,pstring,lstring,ustring);
else
bstring = 'Coefficient'; 
lstring = 'lower 01';
ustring = 'upper 99';
lstring2 = 'lower 05';
ustring2 = 'upper 95';
cnames = strvcat(lstring, lstring2, bstring, ustring2, ustring);
end;

ini.width = 2000;
ini.cnames = cnames;

% print effects estimates

if cflag == 1
vnameso = strvcat(Vname(3:end-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-1,:));
end    
ini.rnames = strvcat('Direct',vnameso);
ini.fmt = '%16.6f';
ini.fid = fid;

% set up print out matrix
printout = direct_out;
mprint(printout,ini);

printout = indirect_out;
ini.rnames = strvcat('Indirect',vnameso);
mprint(printout,ini);

printout = total_out;
ini.rnames = strvcat('Total',vnameso);
mprint(printout,ini);



return;

% <=================== end of sar_gv case


case {'sar_c'} % <=================== log-marginal for spatial autoregressive model

fprintf(fid,'sar_c: no printed output available, this function just produces log-marginal estimates \n');

% ,============ end of sar_c case

case {'sart_g'} % <=================== Gibbs spatial autoregressive Tobit model

nobs = results.nobs;
nvar = results.nvar;
ndraw = results.ndraw;
nomit = results.nomit;


% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
bout = [tmp1'
        pout];

y = results.y;
yhat = results.yhat;
sige = mean(results.sdraw);
tmp1 = std(results.bdraw);
tmp2 = std(results.pdraw);
bstd = [tmp1'
        tmp2];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 for i=1:results.nvar+1;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;

% Compute means, std deviation and upper and lower 0.99 intervals
iter = ndraw-nomit;
p = results.p;
total_out = zeros(p,5);
total_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(total(:,i,:)); % an ndraw by 1 by ntraces matrix
total_mean = mean(tmp);
total_std = std(tmp);
% Bayesian 0.99 credible intervals
% for the cumulative total effects
total_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
total_save(:,i) = total_sum;
bounds = cr_interval(total_sum,0.99);
cmean = mean(total_sum);
smean = std(total_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
if strcmp(results.tflag,'tstat')
total_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
total_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;

% now do indirect effects
indirect_out = zeros(p,5);
indirect_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(indirect(:,i,:)); % an ndraw by 1 by ntraces matrix
indirect_mean = mean(tmp);
indirect_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative indirect effects
indirect_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
indirect_save(:,i) = indirect_sum;
bounds = cr_interval(indirect_sum,0.99);
cmean = mean(indirect_sum);
smean = std(indirect_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
if strcmp(results.tflag,'tstat')
indirect_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
indirect_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;


% now do direct effects
direct_out = zeros(p,5);
direct_save = zeros(ndraw-nomit,p);
for i=1:p;
tmp = squeeze(direct(:,i,:)); % an ndraw by 1 by ntraces matrix
direct_mean = mean(tmp);
direct_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative direct effects
direct_sum = (sum(tmp'))'; % an ndraw by 1 vector
cum_mean = cumsum(mean(tmp));
cum_std = cumsum(std(tmp));
direct_save(:,i) = direct_sum;
bounds = cr_interval(direct_sum,0.99);
cmean = mean(direct_sum);
smean = std(direct_sum);
ubounds = bounds(1,1);
lbounds = bounds(1,2);
if strcmp(results.tflag,'tstat')
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
direct_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;


fprintf(fid,'\n');
fprintf(fid,'Bayesian spatial autoregressive Tobit model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'mean of sige draws = %9.4f \n',sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# censored values  = %6d \n',results.nobsc);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs       = %9.4f   \n',results.time);
fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'***************************************************************\n');

if (results.priorb == 1)
    % non-diffuse prior, so print it
vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bmean results.bstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname(i+1,:));
end;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;

mprint(tmp,pin);
fprintf(fid,'***************************************************************\n');
end;

fprintf(fid,'      Posterior Estimates \n');

 if strcmp(results.tflag,'tstat')
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities
      
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
 else % use p-levels for Bayesian results
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Std Deviation'; pstring = 'p-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;


% now print x-effects estimates
if strcmp(results.tflag,'tstat')
bstring = 'Coefficient'; 
tstring = 't-stat'; 
pstring = 't-prob';
lstring = 'lower 01';
ustring = 'upper 99';
cnames = strvcat(bstring,tstring,pstring,lstring,ustring);
else
bstring = 'Coefficient'; 
lstring = 'lower 01';
ustring = 'upper 99';
lstring2 = 'lower 05';
ustring2 = 'upper 95';
cnames = strvcat(lstring, lstring2, bstring, ustring2, ustring);
end;


ini.cnames = cnames;
ini.width = 2000;

% print effects estimates

if cflag == 1
vnameso = strvcat(Vname(3:end-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-1,:));
end    
ini.rnames = strvcat('Direct',vnameso);
ini.fmt = '%16.6f';
ini.fid = fid;

% set up print out matrix
printout = direct_out;
mprint(printout,ini);

printout = indirect_out;
ini.rnames = strvcat('Indirect',vnameso);
mprint(printout,ini);

printout = total_out;
ini.rnames = strvcat('Total',vnameso);
mprint(printout,ini);



return;

% <=================== end of sart_g case


case {'sarp_g'} % <=================== Gibbs spatial autoregressive Probit model

nobs = results.nobs;
nvar = results.nvar;
  
% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
bout = [tmp1'
        pout];

tmp1 = std(results.bdraw);
tmp2 = std(results.pdraw);
bstd = [tmp1'
        tmp2];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 for i=1:results.nvar+1;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 


% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;
ndraw = results.ndraw;
nomit = results.nomit;

% Compute means, std deviation and upper and lower 0.99 intervals
iter = ndraw-nomit;
p = results.p;
total_out = zeros(p,5);
for i=1:p;
tmp = total(:,i); % an ndraw by 1 vector
total_mean = mean(tmp);
total_std = std(tmp);
% Bayesian 0.99 credible intervals
% for the cumulative total effects
bounds = cr_interval(tmp,0.99);
bounds2 = cr_interval(tmp,0.95);
cmean = total_mean;
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
total_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
total_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;

% now do indirect effects
indirect_out = zeros(p,5);
for i=1:p;
tmp = indirect(:,i); % an ndraw by 1 vector
indirect_mean = mean(tmp);
indirect_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative indirect effects
bounds = cr_interval(tmp,0.99);
bounds2 = cr_interval(tmp,0.95);
cmean = indirect_mean;
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
indirect_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
indirect_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;


% now do direct effects
direct_out = zeros(p,5);
for i=1:p;
tmp = direct(:,i); % an ndraw by 1 vector
direct_mean = mean(tmp);
direct_std = std(tmp);
% Bayesian 0.95 credible intervals
% for the cumulative direct effects
bounds = cr_interval(tmp,0.99);
bounds2 = cr_interval(tmp,0.95);
cmean = direct_mean;
ubounds = bounds(1,1);
lbounds = bounds(1,2);
ubounds2 = bounds2(1,1);
lbounds2 = bounds2(1,2);
if strcmp(results.tflag,'tstat')
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
else
direct_out(i,:) = [lbounds lbounds2 cmean ubounds2 ubounds];
end;
end;


fprintf(fid,'\n');
fprintf(fid,'Bayesian spatial autoregressive Probit model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Nobs, Nvars     = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0, 1 y-values = %6d,%6d \n',results.zip,nobs-results.zip);
fprintf(fid,'ndraws,nomit    = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs    = %9.4f   \n',results.time);
fprintf(fid,'nsteps for TMVN = %6d \n',results.nsteps);
if results.lflag == 0
fprintf(fid,'No lndet approximation used \n');
end;
% put in information regarding Pace and Barry approximations
if results.lflag == 1
fprintf(fid,'Pace and Barry, 1999 MC lndet approximation used \n');
fprintf(fid,'order for MC appr  = %6d  \n',results.order);
fprintf(fid,'iter  for MC appr  = %6d  \n',results.iter);
end;
if results.lflag == 2
fprintf(fid,'Pace and Barry, 1998 spline lndet approximation used \n');
end;

fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'***************************************************************\n');


fprintf(fid,'***************************************************************\n');


 if strcmp(results.tflag,'tstat')
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities
      
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
 else % use p-levels for Bayesian results
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Std Deviation'; pstring = 'p-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;

% now print x-effects estimates
if strcmp(results.tflag,'tstat')
bstring = 'Coefficient'; 
tstring = 't-stat'; 
pstring = 't-prob';
lstring = 'lower 01';
ustring = 'upper 99';
cnames = strvcat(bstring,tstring,pstring,lstring,ustring);
else
bstring = 'Coefficient'; 
lstring = 'lower 01';
ustring = 'upper 99';
lstring2 = 'lower 05';
ustring2 = 'upper 95';
cnames = strvcat(lstring, lstring2, bstring, ustring2, ustring);
end;

ini.width = 2000;
ini.cnames = cnames;


% print effects estimates

if cflag == 1
vnameso = strvcat(Vname(3:end-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-1,:));
end    
ini.rnames = strvcat('Direct',vnameso);
ini.fmt = '%16.6f';
ini.fid = fid;

% set up print out matrix
printout = direct_out;
mprint(printout,ini);

printout = indirect_out;
ini.rnames = strvcat('Indirect',vnameso);
mprint(printout,ini);

printout = total_out;
ini.rnames = strvcat('Total',vnameso);
mprint(printout,ini);



return;

% <=================== end of sarp_g case

case {'sar_gbma'} % <=================== bma for sar models
nmodels = results.nmodels;
nvar = results.nvar;
if nargin < 3
fid = 1;
end;

mout = results.modelsa;
occ = sum(mout);

fmt = [];
for i=1:nvar;
fmt = strvcat(fmt,'%5d');
end;
fmt = strvcat(fmt,'%8.4f');
in.fmt = fmt;
% 
rnames = 'Model';
for i=1:nmodels;
rnames = strvcat(rnames,['model ' num2str(i)]); 
end;
rnames = strvcat(rnames,'#Occurences');
in.rnames = rnames;
cnames = vnames(2:end,:);
cnames = strvcat(cnames,'probs');
in.cnames = cnames;
% 
fprintf(fid,'Model averaging information \n');
in.width = 3000;
in.fid = fid;
[tst1,tst2] = size(results.models);
if tst1 > 1 
out = [results.models
       occ];
mprint(out,in);
else
out = [results.models];
in.rnames = rnames(end-1:end,:);
end;
mprint(out,in);
fprintf(fid,'***************************************************************\n');

% only do this is avg_flag == 1
if results.avg_flag == 1
sige = mean(results.sdraw);
bhat = mean(results.bdraw);
bstd = std(results.bdraw);
bhatp = bhat';
bstdp = bstd';
rho = mean(results.pdraw);
rstd = std(results.pdraw);

fprintf(fid,'\n');
fprintf(fid,'SAR Bayesian Model Averaging Estimates \n');
fprintf(fid,'Dependent Variable   = %16s \n',vnames(1,:));
fprintf(fid,'R-squared            = %9.4f \n',results.rsqr);
fprintf(fid,'sigma^2              = %9.4f \n',sige);
fprintf(fid,'# unique models      = %10d \n',results.munique);
fprintf(fid,'Nobs, Nvars          = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws for BMA       = %6d \n',results.ndraw);
fprintf(fid,'ndraws for estimates = %6d \n',results.ndraw2);
fprintf(fid,'nomit for estimates  = %6d \n',results.nomit2);

if results.time1 ~= 0
fprintf(fid,'time for eigs        = %9.4f \n',results.time1);
end;
if results.time2 ~= 0
fprintf(fid,'time for lndet       = %9.4f \n',results.time2);
end;
if results.time3 ~= 0
fprintf(fid,'time for BMA sampling= %9.4f \n',results.time3);
end;
if results.time4 ~= 0
fprintf(fid,'time for estimates   = %9.4f \n',results.time4);
end;


if results.lflag == 0
fprintf(fid,'No lndet approximation used \n');
end;
% put in information regarding Pace and Barry approximations
if results.lflag == 1
fprintf(fid,'Pace and Barry, 1999 MC lndet approximation used \n');
fprintf(fid,'order for MC appr  = %6d  \n',results.order);
fprintf(fid,'iter  for MC appr  = %6d  \n',results.iter);
end;
if results.lflag == 2
fprintf(fid,'Pace and Barry, 1998 spline lndet approximation used \n');
end;

fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bmean results.bstd];

cnames = strvcat(bstring,tstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname(i+1,:));
end;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = rnames;
fprintf(fid,'***************************************************************\n');

mprint(tmp,pin);

fprintf(fid,'***************************************************************\n');
fprintf(fid,'      Posterior Estimates \n');


% column labels for printing results
vstring = 'Variable';
bstring = strvcat('Coefficient','std dev');

ball = [bhatp bstdp
        rho   rstd];


if strcmp(results.tflag,'tstat')
% now print coefficient estimates, t-statistics and probabilities
tstat = [bhatp./bstdp
         rho/rstd];
tout = norm_prb(tstat); % find asymptotic z (normal) probabilities    
tmp = [ball(:,1) tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
rnames = 'Variable';
 for i=1:nvar
  rnames = strvcat(rnames,vnames(i+1,:));
 end;
rnames = strvcat(rnames,'rho');

in.cnames = cnames;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
 else % use p-levels for Bayesian results
 draws = [results.bdraw results.pdraw];

 for i=1:results.nvar+1;
 if ball(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw2 - results.nomit2));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw2 - results.nomit2));
 end; % end of if - else
 end; % end of for loop

tmp = [ball tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Std Deviation'; pstring = 'p-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
rnames = 'Variable';
 for i=1:nvar
  rnames = strvcat(rnames,vnames(i+1,:));
 end;
rnames = strvcat(rnames,'rho');
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;

end; % end of avg_flag == 1

return;


otherwise
error('results structure not known by prt_sar function');
end;





function bounds = cr_interval(adraw,hperc)
% PURPOSE: Computes an hperc-percent credible interval for a vector of MCMC draws
% --------------------------------------------------------------------
% Usage: bounds = cr_interval(draws,hperc);
% where draws = an ndraw by nvar matrix
%       hperc = 0 to 1 value for hperc percentage point
% --------------------------------------------------------------------
% RETURNS:
%         bounds = a 1 x 2 vector with 
%         bounds(1,1) = 1-hperc percentage point
%         bounds(1,2) = hperc percentage point
%          e.g. if hperc = 0.95
%          bounds(1,1) = 0.05 point for 1st vector in the matrix
%          bounds(1,2) = 0.95 point  for 1st vector in the matrix
%          bounds(2,1) = 0.05 point for 2nd vector in the matrix
%          bounds(2,2) = 0.05 point for 2nd vector in the matrix
%          ...
% --------------------------------------------------------------------

% written by:
% James P. LeSage, 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


% This function takes a vector of MCMC draws and calculates
% an hperc-percent credible interval
[ndraw,ncols]=size(adraw);
botperc=round((0.50-hperc/2)*ndraw);
topperc=round((0.50+hperc/2)*ndraw);
bounds = zeros(ncols,2);
for i=1:ncols;
temp = sort(adraw(:,i),1);
bounds(i,:) =[temp(topperc,1) temp(botperc,1)];
end;


