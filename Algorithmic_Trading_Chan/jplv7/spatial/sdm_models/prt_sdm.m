function prt_sdm(results,vnames,fid)
% PURPOSE: Prints output using sdm results structures
%---------------------------------------------------
% USAGE: prt_sdm(results,vnames,fid)
% Where: results = a structure returned by sdm, sdm_g, sdm_gc, etc.
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_sdm(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial regression results
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if ~isstruct(results)
 error('prt_sdm requires structure argument');
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
 error('Wrong # of arguments to prt_sdm');
end;


nobs = results.nobs;
cflag = results.cflag;
p = results.p;
if cflag == 1
    nvars = p+1;
elseif cflag == 0
    nvars = p;
end;


if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvars+1
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
     for i=1:nvars-1
        tmp = ['variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
     for i=1:(nvars-1)
        tmp = ['W*variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
 
    elseif cflag == 0 % no constant term

     for i=1:nvars
        tmp = ['variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
     for i=1:nvars
        tmp = ['W*variable ',num2str(i)];
        Vname = strvcat(Vname,tmp);
     end;
    end;
 
     
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');

elseif (nflag == 1) % the user supplied variable names
    if cflag == 0 % no constant term
    Vname = 'Variable';
     for i=1:nvars
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
     for i=1:nvars;
        Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
     elseif cflag == 1 % a constant term
     Vname = 'Variable';
     for i=1:nvars
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
     for i=2:nvars;
        Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
    end; % end of cflag issue       
 
end; % end of nflag issue


switch results.meth

case {'sdm'} % <=================== spatial durbin model

ndraw = results.ndraw;

% do effects estimates
% =======================================================
% a set of draws for the effects/impacts distribution
total    = results.total;
indirect = results.indirect;
direct   = results.direct;


% Compute means, std deviation and upper and lower 0.99 intervals
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
fprintf(fid,'Spatial Durbin model\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'log-likelihood     = %16.8g  \n',results.lik);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# iterations       = %6d     \n',results.iter);
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
fprintf(fid,'# draws used       = %10d  \n',results.ndraw);
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
vnameso = strvcat(Vname(3:nvars+1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:nvars+1,:));
end

ini.rnames = strvcat('Direct',vnameso);

fmt = '%16.6f';
ini.fmt = fmt;


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


        
% <=================== end of sdm case


case {'sdm_g'} % <=================== spatial durbin model MCMC



% find posterior means
    bout = [results.beta
            results.rho];
    sige = results.sige;
    tmp1 = std(results.bdraw);
    tmp2 = std(results.pdraw);
    bstd = [tmp1'
        tmp2];  


if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 [junk nk] = size(results.bdraw);
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 [junk nk] = size(draws);
 for i=1:nk;
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
ndraw = results.ndraw;
nomit = results.nomit;
iter = ndraw-nomit;
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
fprintf(fid,'Bayesian Spatial Durbin model\n');
if results.novi == 1
    fprintf(fid,'Homoscedastic version \n');
elseif results.novi == 0
    fprintf(fid,'Heteroscedastic model \n');
end;    
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'mean of sige draws = %9.4f   \n',sige);
fprintf(fid,'sige, epe/(n-k)    = %9.4f   \n',results.sigma);
if (results.rdraw == 0 & results.novi == 0)
fprintf(fid,'r-value            = %6d   \n',results.r);
elseif (results.rdraw ~= 0  & results.novi == 0)
fprintf(fid,'mean of rdraws     = %9.4f \n',mean(results.rdraw));
fprintf(fid,'gam(m,k) prior     = %6d,%6d \n',results.m,results.k);
end;  
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,nk);
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
if results.time4 ~= 0
fprintf(fid,'time for effects   = %9.4f \n',results.time4);
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
fprintf(fid,'min and max rho= %9.4f,%9.4f \n',results.rmin,results.rmax);
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
vnameso = strvcat(Vname(3:p+2,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:p+1,:));
end;   


ini.rnames = strvcat('Direct',vnameso);
fmt = '%16.6f';
ini.fmt = fmt;

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
   
% <=================== end of sdm_g case
     


case {'sdmp_g'} % <=================== spatial durbin probit model MCMC



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
 [junk nk] = size(results.bdraw);
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 [junk nk] = size(draws);
 for i=1:nk;
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
fprintf(fid,'Bayesian Spatial Durbin Probit model\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'# 0, 1 y-values    = %6d,%6d \n',results.zip,nobs-results.zip);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,nk);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'total time in secs = %9.4f   \n',results.time);
fprintf(fid,'time for sampling  = %9.4f \n',results.time3);

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

fprintf(fid,'min and max rho= %9.4f,%9.4f \n',results.rmin,results.rmax);
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
ini.cnames = cnames;% print effects estimates

if cflag == 1
vnameso = strvcat(Vname(3:3+p-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:2+p-1,:));
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
   
% <=================== end of sdmp_g case
     
case {'sdmt_g'} % <=================== spatial durbin tobit model MCMC


% find posterior means
    tmp1 = mean(results.bdraw);
    pout = mean(results.pdraw);
    bout = [tmp1'
        pout];
    sige = mean(results.sdraw);
    tmp1 = std(results.bdraw);
    tmp2 = std(results.pdraw);
    bstd = [tmp1'
        tmp2];  


if strcmp(results.tflag,'tstat')
    [junk nk] = size(results.bdraw);
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw];
 [junk nk] = size(draws);
 for i=1:nk;
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
ndraw = results.ndraw;
nomit = results.nomit;
iter = ndraw-nomit;
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
fprintf(fid,'Bayesian Spatial Durbin tobit model\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'sige               = %9.4f \n',sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,nk);
fprintf(fid,'# censored values  = %6d \n',results.nobsc);
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

fprintf(fid,'min and max rho= %9.4f,%9.4f \n',results.rmin,results.rmax);
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
vnameso = strvcat(Vname(3:3+p-1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:2+p-1,:));
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
   
% <=================== end of sdmt_g case

case {'sdm_gmm'} % <=================== spatial durbin model GMM estimation

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
indirect_out(i,:) = [cmean  cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds  ];
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
direct_out(i,:) = [cmean cmean./smean tdis_prb(cmean./smean,nobs) lbounds ubounds];
end;



fprintf(fid,'\n');
fprintf(fid,'Spatial Durbin model GMM estimation\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.time5 ~= 0
fprintf(fid,'time for x-impacts = %9.4f \n',results.time5);
fprintf(fid,'# draws used       = %10d  \n',results.ndraw);
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
vnameso = strvcat(Vname(3:nvars+1,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:nvars+1,:));
end

ini.rnames = strvcat('Direct',vnameso);

fmt = '%18.6f';
for jj=1:4;
fmt = strvcat(fmt,'%16.6f');
end;
ini.fmt = fmt;


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


otherwise
error('results structure not known by prt_sdm function');
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

% Written by J.P. LeSage

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



