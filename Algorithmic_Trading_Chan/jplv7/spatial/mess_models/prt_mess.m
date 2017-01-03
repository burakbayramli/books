function prt_mess(results,vnames,fid)
% PURPOSE: Prints output using MESS models results structures
%---------------------------------------------------
% USAGE: prt_mess(results,vnames,fid)
% Where: results = a structure returned by a mess regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_mess(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the mess model results
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
 error('prt_mess requires structure argument');
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
 error('Wrong # of arguments to prt_mess');
end;

switch results.meth


case {'mess'} % <=================== mess model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 end;  
end; % end of nflag issue


fprintf(fid,'\n');
fprintf(fid,'Matrix Exponential Spatial Specification Model\n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.sige);
fprintf(fid,'log-likelihood     = %16.8g  \n',results.lik);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of neighbors     = %6d     \n',results.neigh);
fprintf(fid,'rho value used     = %9.4f   \n',results.rho);
fprintf(fid,'q value used       = %6d     \n',results.q);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.htime ~= 0
fprintf(fid,'time for hessian   = %9.4f \n',results.htime);
end;
if results.ntime ~= 0
fprintf(fid,'time for neighbors = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

bout = [results.bhat
        results.alpha];

if results.hflag == 1
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
else % we don't have t-statistics
tmp = [bout ];
bstring = 'Coefficient'; ;
cnames = strvcat(bstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
end;

return;
        
% <=================== end of mess case


case {'mess_g'} % <=================== mess_g model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.adraw);
bout = [tmp1'
        pout];

y = results.y;
sige = mean(results.sdraw);
tmp1 = std(results.bdraw);
tmp2 = std(results.adraw);
bstd = [tmp1'
        tmp2];
if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Specification\n');
fprintf(fid,'rho and # neighbors fixed apriori\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of neighbors     = %6d     \n',results.neigh);
fprintf(fid,'rho value used     = %9.4f   \n',results.rho);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'acceptance rate    = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for neighbors = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end,:));

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
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of mess_g case

case {'messt_g'} % <=================== messt_g model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.adraw);
bout = [tmp1'
        pout];

y = results.y;
sige = mean(results.sdraw);
tmp1 = std(results.bdraw);
tmp2 = std(results.adraw);
bstd = [tmp1'
        tmp2];
if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Tobit Specification\n');
fprintf(fid,'rho and # neighbors fixed apriori\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Conventional R^2   = %9.4f   \n',results.rsqr);
fprintf(fid,'Imputed R^2        = %9.4f   \n',results.rsqri);
fprintf(fid,'sigma^2            = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of censored obs  = %6d     \n',results.nobsc);
fprintf(fid,'# of neighbors     = %6d     \n',results.neigh);
fprintf(fid,'rho value used     = %9.4f   \n',results.rho);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'acceptance rate    = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for neighbors = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end,:));

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
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of messt_g case

case {'messp_g'} % <=================== messp_g model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = mean(results.bdraw);
pout = mean(results.adraw);
bout = [tmp1'
        pout];

y = results.y;
tmp1 = std(results.bdraw);
tmp2 = std(results.adraw);
bstd = [tmp1'
        tmp2];
if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Probit Specification\n');
fprintf(fid,'rho and # neighbors fixed apriori\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Estrella R-squared = %9.4f   \n',results.rsqr);
fprintf(fid,'McFadden R-squared = %9.4f   \n',results.r2mf);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# 0, 1 y-values    = %6d,%6d \n',results.zip,nobs-results.zip);
fprintf(fid,'# of neighbors     = %6d     \n',results.neigh);
fprintf(fid,'rho value used     = %9.4f   \n',results.rho);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'acceptance rate    = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for neighbors = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end,:));

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
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of messp_g case

case {'mess_g1'} % <=================== mess_g1 model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'#neighbors');

else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial alpha, rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'#neighbors');

end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial hyperparameter names
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'#neighbors');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'#neighbors');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = results.bmean;
pout = results.amean;
rout = results.mmean;
bout = [tmp1
        pout
        rout];

y = results.y;
sige = results.smean;
tmp1 = results.bstd;
tmp2 = results.astd;
tmp3 = results.mstd;
bstd = [tmp1
        tmp2
        tmp3];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw results.mdraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Specification\n');
fprintf(fid,'rho fixed and # neighbors estimated\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'rho value used     = %9.4f     \n',results.rho);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'min,max neighbors  = %6d,%6d \n',results.mmin,results.mmax);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'alpha accept rate  = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for setup     = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end-1,:));
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

% now print coefficient estimates, t-statistics and probabilities
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of mess_g1 case



case {'mess_g2'} % <=================== mess_g2 model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial alpha, rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');

end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial hyperparameter names
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = results.bmean;
pout = results.amean;
rout = results.rmean;
bout = [tmp1
        pout
        rout];

y = results.y;
sige = results.smean;
tmp1 = results.bstd;
tmp2 = results.astd;
tmp3 = results.rstd;
bstd = [tmp1
        tmp2
        tmp3];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw results.rdraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Specification\n');
fprintf(fid,'# neighbors fixed and rho estimated\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# neighbors used   = %6d     \n',results.neigh);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'min,max rho used   = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'alpha accept rate  = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for setup     = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end-1,:));

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

% now print coefficient estimates, t-statistics and probabilities
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of mess_g2 case


case {'mess_g3'} % <=================== mess_g3 model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neighbors');

else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial alpha, rho, and neighbors parameter names
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neighbors');

end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial hyperparameter names
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neighbors');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neighbors');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = results.bmean;
pout = results.amean;
rout = results.rmean;
dout = results.mmean;
bout = [tmp1
        pout
        rout
        dout];

y = results.y;
sige = results.smean;
tmp1 = results.bstd;
tmp2 = results.astd;
tmp3 = results.rstd;
tmp4 = results.mstd;
bstd = [tmp1
        tmp2
        tmp3
        tmp4];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw results.rdraw results.mdraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Specification\n');
fprintf(fid,'rho and # neighbors estimated\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable  = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared           = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared        = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2             = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars         = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'min,max # neighbors = %6d,%6d \n',results.mmin,results.mmax);
fprintf(fid,'min,max rho used    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'q value used        = %6d     \n',results.q);
fprintf(fid,'ndraws,nomit        = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'alpha accept rate   = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs  = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling   = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for setup      = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end-2,:));

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

% now print coefficient estimates, t-statistics and probabilities
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of mess_g3 case


case {'messv_g3'} % <=================== messv_g3 model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neighbors');

else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial alpha, rho, and neighbors parameter names
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neighbors');

end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial hyperparameter names
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neighbors');
   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neighbors');
   end; % end of if-else
 end;  
end; % end of nflag issue


% find posterior means
tmp1 = results.bmean;
pout = results.amean;
rout = results.rmean;
dout = results.mmean;
bout = [tmp1
        pout
        rout
        dout];

y = results.y;
sige = results.smean;
tmp1 = results.bstd;
tmp2 = results.astd;
tmp3 = results.rstd;
tmp4 = results.mstd;
bstd = [tmp1
        tmp2
        tmp3
        tmp4];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw results.rdraw results.mdraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian heteroscedastic MESS estimates\n');
fprintf(fid,'rho and # neighbors estimated\n');

if (nflag == 1)
fprintf(fid,'Dependent Variable  = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared           = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared        = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2             = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars         = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'min,max # neighbors = %6d,%6d \n',results.mmin,results.mmax);
fprintf(fid,'min,max rho used    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'q value used        = %6d     \n',results.q);
if results.rvdraw == 0
fprintf(fid,'r-value            = %6d   \n',results.rval);
else
fprintf(fid,'mean of rdraws     = %9.4f \n',mean(results.rvdraw));
fprintf(fid,'gam(m,k) prior     = %6d,%6d \n',results.m,results.k);
end;    
fprintf(fid,'ndraws,nomit        = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'alpha accept rate   = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs  = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling   = %9.4f \n',results.stime);
end;
if results.ntime ~= 0
fprintf(fid,'time for setup      = %9.4f \n',results.ntime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end-2,:));

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

% now print coefficient estimates, t-statistics and probabilities
tmp = [bout bstd tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of messv_g3 case

case {'messt_g3'} % <=================== messt_g3 model

nobs = results.nobs;
nvar = results.nvar;

% special handling of vnames
Vname = 'Variable';
if results.xflag == 1
 for i=1:2*(nvar-1)+1;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neigh');

else
 for i=1:nvar;
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
% add spatial rho parameter name
Vname = strvcat(Vname,'alpha');
Vname = strvcat(Vname,'rho');
Vname = strvcat(Vname,'neigh');

end;

if (nflag == 1) % the user supplied variable names
 if results.xflag == 1
   Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    for i=2:nvar
    Vname = strvcat(Vname,['W-' vnames(i+1,:)]);
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neigh');

   end; % end of if-else
 else
    Vname = 'Variable';
  [tst_n nsize] = size(vnames);
   if tst_n ~= nvar+1
   fprintf(fid,'Wrong # of variable names in prt_mess -- check vnames argument \n');
   nflag = 0;
   fprintf(fid,'will use generic variable names \n');
   else
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
    % add spatial rho parameter name
    Vname = strvcat(Vname,'alpha');
    Vname = strvcat(Vname,'rho');
    Vname = strvcat(Vname,'neigh');
   end; % end of if-else
 end;  
end; % end of nflag issue

% find posterior means
tmp1 = results.bmean;
pout = results.amean;
rout = results.rmean;
dout = results.mmean;
bout = [tmp1
        pout
        rout
        dout];

y = results.y;
sige = results.smean;
tmp1 = results.bstd;
tmp2 = results.astd;
tmp3 = results.rstd;
tmp4 = results.mstd;
bstd = [tmp1
        tmp2
        tmp3
        tmp4];

if strcmp(results.tflag,'tstat')
 tstat = bout./bstd;
 % find t-stat marginal probabilities
 tout = tdis_prb(tstat,results.nobs);
 results.tstat = bout./bstd; % trick for printing below
else % find plevels
   draws = [results.bdraw results.adraw results.rdraw results.mdraw];
   [junk kk] = size(draws);
 for i=1:kk;
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
fprintf(fid,'Bayesian Matrix Exponential Spatial Tobit Specification\n');
fprintf(fid,'rho and # neighbors estimated \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'Conventional R^2   = %9.4f   \n',results.rsqr);
fprintf(fid,'Imputed R^2        = %9.4f   \n',results.rsqri);
fprintf(fid,'sigma^2            = %9.4f   \n',results.smean);
fprintf(fid,'Nobs, Nvars        = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of censored obs  = %6d     \n',results.nobsc);
fprintf(fid,'min,max # neighbors = %6d,%6d \n',results.mmin,results.mmax);
fprintf(fid,'min,max rho used    = %9.4f,%9.4f \n',results.rmin,results.rmax);
fprintf(fid,'q value used       = %6d     \n',results.q);
fprintf(fid,'ndraws,nomit       = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'acceptance rate    = %9.4f \n',results.accept);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
if results.stime ~= 0
fprintf(fid,'time for sampling  = %9.4f \n',results.stime);
end;

if results.xflag == 0
fprintf(fid,'No spatially lagged X variables \n');
end;

fprintf(fid,'***************************************************************\n');

vstring = 'Variable';
bstring = 'Prior Mean';
tstring = 'Std Deviation';

tmp = [results.bprior results.bpstd];
    tmp = [tmp
           results.palpha sqrt(results.acov)];
cnames = strvcat(bstring,tstring);
rnames = vstring;

pin.fmt = '%16.6f';
pin.fid = fid;
pin.cnames = cnames;
pin.rnames = strvcat(rnames,Vname(2:end-2,:));

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
bstring = 'Coefficient'; tstring = 'std deviation'; pstring = 'P-level';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

end;

return;
        
% <=================== end of messt_g3 case




otherwise
error('results structure not known by prt_mess function');
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

