function prt_gmm(results,vnames,fid)
% PURPOSE: Prints output of sem_gmm, sar_gmm, sac_gmm
%---------------------------------------------------
% USAGE: prt_semgm(results,vnames,fid)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_gmm(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial regression results
% --------------------------------------------------
% SEE ALSO: sem_gmm, sar_gmm, sac_gmm
%---------------------------------------------------   

% written by: Shawn Bucholtz
% SBUCHOLTZ@ers.usda.gov
% USDA-ERS-ISD-ADB
% adopted from code written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('prt_gmm requires structure argument');
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
 error('Wrong # of arguments to prt_gmm');
end;

nvar = results.nvar;
nobs = results.nobs;

% handling of vnames
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
if strcmp(results.meth,'sem_gmm');
% add spatial parameter name
    Vname = strvcat(Vname,'lambda');
elseif strcmp(results.meth,'sem2_gmm');
    Vname = strvcat(Vname,'lambda1');
    Vname = strvcat(Vname,'lambda2');
elseif strcmp(results.meth,'sac_gmm');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'lambda');
elseif strcmp(results.meth,'sar_gmm');
    Vname = strvcat(Vname,'rho');    
end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_gmm -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
if strcmp(results.meth,'sem_gmm');
% add spatial parameter name
    Vname = strvcat(Vname,'lambda');
elseif strcmp(results.meth,'sem2_gmm');
    Vname = strvcat(Vname,'lambda1');
    Vname = strvcat(Vname,'lambda2');
elseif strcmp(results.meth,'sac_gmm');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'lambda');
elseif strcmp(results.meth,'sar_gmm');
    Vname = strvcat(Vname,'rho');    
end;
end; % end of if-else
end; % end of nflag issue

switch results.meth

case {'sem_gmm'} % <=================== GMM spatial error model one weight matrix


nobs = results.nobs;
nvar = results.nvar;


fprintf(fid,'\n');
    fprintf(fid,'Generalized Moments Estimation of Spatial Error Model\n');
    fprintf(fid,'Model with 1 weight matrix \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable     = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'GM sigma^2         = %9.4f   \n',results.GMsige);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'optimization time  = %9.4f   \n',results.time1);
fprintf(fid,'total time         = %9.4f   \n',results.time);
fprintf(fid,'# of iterations    = %9d     \n',results.iter);

fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.lambda];
        
tstats = [results.tstat
          results.lambdatstat];

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstats); % find asymptotic z (normal) probabilities
tmp = [bout tstats tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-stat'; pstring = 'probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);




case {'sem2_gmm'} % <=================== GMM spatial error model two weight matrices


nobs = results.nobs;
nvar = results.nvar;

fprintf(fid,'\n');
    fprintf(fid,'Generalized Moments Estimation of Spatial Error Model\n');
    fprintf(fid,'Model with 2 weight matrices \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable     = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'GM sigma^2         = %9.4f   \n',results.GMsige);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'optimization time  = %9.4f   \n',results.time1);
fprintf(fid,'total time         = %9.4f   \n',results.time);
fprintf(fid,'# of iterations    = %9d     \n',results.iter);


fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.lambda];
        
tstats = [results.tstat
          results.lambdatstat];

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstats); % find asymptotic z (normal) probabilities
tmp = [bout tstats tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-stat'; pstring = 'probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

case {'sac_gmm'} % <=================== GMM general spatial model 


cflag = results.cflag;
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
    fprintf(fid,'Generalized Moments Estimation of general spatial model \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable     = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'GM sigma^2         = %9.4f   \n',results.GMsige);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'optimization time  = %9.4f   \n',results.time1);
fprintf(fid,'total time         = %9.4f   \n',results.time);
fprintf(fid,'# of iterations    = %9d     \n',results.iter);
fprintf(fid,'time for impacts   = %9.4f \n',results.time1);
fprintf(fid,'# draws  x-impacts = %9d   \n',results.ndraw);



fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho
        results.lam];
        
tstats = [results.tstat
          results.rhotstat
          results.lambdatstat];

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstats); % find asymptotic z (normal) probabilities
tmp = [bout tstats tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-stat'; pstring = 'probability';
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


case {'sar_gmm'} % <=================== GMM spatial lag model 

cflag = results.cflag;
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
    fprintf(fid,'Generalized Moments Estimation of Spatial Autoregressive Model\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable     = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'EGLS sigma^2       = %9.4f   \n',results.sige);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'total time         = %9.4f   \n',results.time);
fprintf(fid,'time for x-impacts = %9.4f \n',results.time1);
fprintf(fid,'# draws  x-impacts = %9d   \n',results.ndraw);

fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho];
        
tstats = [results.tstat
          results.rhotstat];

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstats); % find asymptotic z (normal) probabilities
tmp = [bout tstats tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-stat'; pstring = 'probability';
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





otherwise
error('results structure not known by prt_gmm function');
end;
