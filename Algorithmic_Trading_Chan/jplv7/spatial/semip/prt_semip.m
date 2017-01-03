function prt_semip(results,vnames,fid)
% PURPOSE: Prints output using semip model results structures
%---------------------------------------------------
% USAGE: prt_semip(results,vnames,fid)
% Where: results = a structure returned by semip_g.m, or semip_gc.m
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_semip(results,[],fid) to print to a file with no vnames               
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
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('prt_semip requires structure argument');
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
 error('Wrong # of arguments to prt_semip');
end;


nvar = results.nvar;
nobs = results.nobs;

% handling of vnames
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_semip -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
 end; % end of if-else
end; % end of nflag issue



switch results.meth


case {'semip_g', 'semip_gc'} % <=================== probit regression model with individual effects
                %                      that follow a spatial AR model 

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

if (nflag == 1) % the user supplied variable names
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_sem -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
 end; % end of if-else
end; % end of nflag issue

  
% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
bout = [tmp1'
        pout];

y = results.y;
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

e = y - results.yhat;
sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian probit model with individual spatial effects \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
%fprintf(fid,'R-squared          = %9.4f \n',rsqr);
fprintf(fid,'sigma^2            = %9.4f \n',sige);
if results.rdraw == 0
fprintf(fid,'r-value            = %6d   \n',results.r);
else
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
if results.dflag == 0
fprintf(fid,'Griddy Gibbs with inversion used for rho \n');
else
fprintf(fid,'Metropolis-Hastings used for rho \n');
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

fprintf(fid,'min and max lambda = %9.4f,%9.4f \n',results.rmin,results.rmax);

% only print priors if non-diffuse
if (results.bflag == 1)
    % non-diffuse prior, so print it
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
end;
fprintf(fid,'***************************************************************\n');
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

return;

% <=================== end of semip_g case

case {'semit_g'} % <=================== regression model with individual effects
                %                      that follow a spatial AR model 

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

if (nflag == 1) % the user supplied variable names
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_sem -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
% add spatial rho parameter name
    Vname = strvcat(Vname,'rho');
 end; % end of if-else
end; % end of nflag issue

  
% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.pdraw);
bout = [tmp1'
        pout];

y = results.y;
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

e = y - results.yhat;
sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian tobit model with individual spatial effects \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
%fprintf(fid,'R-squared          = %9.4f \n',rsqr);
fprintf(fid,'sigma^2            = %9.4f \n',sige);
if results.rdraw == 0
fprintf(fid,'r-value            = %6d   \n',results.r);
else
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

fprintf(fid,'min and max lambda = %9.4f,%9.4f \n',results.rmin,results.rmax);
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

return;

% <=================== end of semit_g case


otherwise
error('results structure not known by prt_semip function');
end;


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



