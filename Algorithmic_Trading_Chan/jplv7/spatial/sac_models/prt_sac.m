function prt_sac(results,vnames,fid)
% PURPOSE: Prints output using sac model results structures
%---------------------------------------------------
% USAGE: prt_sac(results,vnames,fid)
% Where: results = a structure returned by sac, sac_g, etc.
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_spat(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial regression results
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
 error('prt_sac requires structure argument');
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
 error('Wrong # of arguments to prt_sac');
end;

nvar = results.nvar;
nobs = results.nobs;
cflag = results.cflag;

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
    Vname = strvcat(Vname,'lambda');

elseif (nflag == 1) % the user supplied variable names
    if cflag == 0 % no constant term
    Vname = 'Variable';
     for i=1:nvar
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
        Vname = strvcat(Vname,'lambda');
     elseif cflag == 1 % a constant term
     Vname = 'Variable';
     for i=1:nvar
        Vname = strvcat(Vname,vnames(i+1,:));
     end;
    % add spatial rho parameter name
        Vname = strvcat(Vname,'rho');
        Vname = strvcat(Vname,'lambda');
    end; % end of cflag issue       
 
end; % end of nflag issue


switch results.meth


case {'sac'} % <=================== general spatial model

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
fprintf(fid,'General Spatial Model Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f \n',results.sige);
fprintf(fid,'log-likelihood     = %16.8g \n',results.lik);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# iterations       = %6d \n',results.iter);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
fprintf(fid,'time for optimiz   = %9.4f \n',results.time4);
fprintf(fid,'time for lndet     = %9.4f \n',results.time1);
fprintf(fid,'time for t-stat    = %9.4f \n',results.time3);
fprintf(fid,'time for impacts   = %9.4f \n',results.time5);
fprintf(fid,'# draws  x-impacts = %9d   \n',results.ndraw);

fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho
        results.lam];
    
    
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
vnameso = strvcat(Vname(3:end-2,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-2,:));    
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

        

% <=================== end of sac case

case {'sac_g'} % <=================== Gibbs general spatial model


nobs = results.nobs;
nvar = results.nvar;
ndraw = results.ndraw - results.nomit;

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




bout = [results.beta
        results.rho
        results.lambda];


    tmp1 = std(results.bdraw);
    tmp2 = std(results.pdraw);
    tmp3 = std(results.ldraw);
    bstd = [tmp1'
            tmp2
            tmp3];  

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw results.ldraw];
 for i=1:results.nvar+2;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 



fprintf(fid,'\n');
fprintf(fid,'Bayesian General Spatial Model Estimates \n');
if results.novi == 1
    fprintf(fid,'Homoscedastic version \n');
elseif results.novi == 0
    fprintf(fid,'Heteroscedastic model \n');
end;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'min and max lambda = %9.4f,%9.4f \n',results.lmin,results.lmax);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
fprintf(fid,'time for eigs      = %9.4f \n',results.time1);
fprintf(fid,'time for lndet     = %9.4f \n',results.time2);
fprintf(fid,'MCMC sampling time = %9.4f \n',results.time3);
fprintf(fid,'time for x-effects = %9.4f \n',results.time4);



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

fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho
        results.lambda];
       
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
vnameso = strvcat(Vname(3:end-2,:));
elseif cflag == 0
vnameso = strvcat(Vname(2:end-2,:));    
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



return;

% <=================== end of sar_g case


case {'sact_g'} % <=================== Gibbs general spatial Tobit model

nobs = results.nobs;
nvar = results.nvar;

% handling of vnames
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
% add spatial rho parameter name
    Vname = strvcat(Vname,'lambda');

if (nflag == 1) % the user supplied variable names
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'lambda');
 end; % end of if-else
end; % end of nflag issue

  
% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
lout = mean(results.ldraw);
bout = [tmp1'
        pout
        lout];

y = results.y;
sige = mean(results.sdraw);
tmp1 = std(results.bdraw);
tmp2 = std(results.pdraw);
tmp3 = std(results.ldraw);
bstd = [tmp1'
        tmp2
        tmp3];

if strcmp(results.pflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw results.ldraw];
 for i=1:results.nvar+2;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

yhat = results.yhat;
e = y - yhat;
sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared
e = results.ymean - yhat;
sigu = e'*e;
ym = results.ymean - mean(results.ymean);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqri = 1.0 - rsqr1/rsqr2; % non-conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian general spatial Tobit model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f \n',rsqr);
fprintf(fid,'Imputed R-squared  = %9.4f \n',rsqri);
fprintf(fid,'sigma^2            = %9.4f \n',sige);
if results.rdraw == 0
fprintf(fid,'r-value            = %6d   \n',results.r);
else
fprintf(fid,'mean of rdraws     = %9.4f \n',mean(results.rdraw));
fprintf(fid,'gam(m,k) prior     = %6d,%6d \n',results.m,results.k);
end;    
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# censored values  = %6d \n',results.nobsc);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'accept rho rate    = %9.4f \n',results.acceptr);
fprintf(fid,'accept lam rate    = %9.4f \n',results.acceptl);
fprintf(fid,'time in secs       = %9.4f   \n',results.time);
fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'min and max lambda = %9.4f,%9.4f \n',results.lmin,results.lmax);
fprintf(fid,'***************************************************************\n');

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
fprintf(fid,'      Posterior Estimates \n');

 if strcmp(results.pflag,'tstat')
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

return;

% end of sact_g case

case {'sacp_g'} % <=================== Gibbs general spatial Probit model

nobs = results.nobs;
nvar = results.nvar;

% handling of vnames
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
% add spatial rho parameter name
    Vname = strvcat(Vname,'lambda');

if (nflag == 1) % the user supplied variable names
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'lambda');
 end; % end of if-else
end; % end of nflag issue

% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
lout = mean(results.ldraw);
bout = [tmp1'
        pout
        lout];

y = results.y;
sige = mean(results.sdraw);
tmp1 = std(results.bdraw);
tmp2 = std(results.pdraw);
tmp3 = std(results.ldraw);
bstd = [tmp1'
        tmp2
        tmp3];

if strcmp(results.pflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
 draws = [results.bdraw results.pdraw results.ldraw];
 for i=1:results.nvar+2;
 if bout(i,1) > 0
 cnt = find(draws(:,i) > 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 else
 cnt = find(draws(:,i) < 0);
 tout(i,1) = 1 - (length(cnt)/(results.ndraw-results.nomit));
 end; % end of if - else
 end; % end of for loop
end; 

yhat = results.yhat;

fprintf(fid,'\n');
fprintf(fid,'Bayesian general spatial Probit model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'McFadden R^2    = %9.4f \n',results.r2mf);
fprintf(fid,'Estrella R^2    = %9.4f \n',results.rsqr);
fprintf(fid,'sigma^2         = %9.4f \n',sige);
if results.rdraw == 0
fprintf(fid,'r-value         = %6d   \n',results.r);
else
fprintf(fid,'mean of rdraws  = %9.4f \n',mean(results.rdraw));
fprintf(fid,'gam(m,k) prior  = %6d,%6d \n',results.m,results.k);
end; 
fprintf(fid,'Nobs, Nvars     = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0, 1 y-values = %6d,%6d \n',results.zip,nobs-results.zip);
fprintf(fid,'ndraws,nomit    = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'rho accept rate = %9.4f \n',results.acceptr);
fprintf(fid,'lam accept rate = %9.4f \n',results.acceptl);
fprintf(fid,'time in secs    = %9.4f   \n',results.time);
fprintf(fid,'min and max rho = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'***************************************************************\n');

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
fprintf(fid,'      Posterior Estimates \n');

 if strcmp(results.pflag,'tstat')
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

 return;
 
% <=================== end of sacp_g case

otherwise
error('results structure not known by prt_spat function');
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



