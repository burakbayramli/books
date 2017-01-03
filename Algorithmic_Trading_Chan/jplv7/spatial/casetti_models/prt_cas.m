function prt_cas(results,vnames,fid)
% PURPOSE: Prints output using spatial expansion results structures
%---------------------------------------------------
% USAGE: prt_cas(results,vnames,fid)
% Where: results = a structure returned by spatial expansion 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_cas(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial expansion results
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
 error('prt_cas requires structure argument');
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
 error('Wrong # of arguments to prt_cas');
end;

switch results.meth


case {'casetti'} % <=================== casetti expansion model

nvar = results.nvar;
nobs = results.nobs;
expansion = results.exp;

if expansion == 0 % case of x-y expansion
xvar = nvar-1;
yvar = nvar-1;

% special handling of vnames
% make up generic variable names
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,['variable',num2str(i)]);
end;
 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,vnames(i+1,:));
end;
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end; % end of if-else
end; % end of nflag issue


bout = results.b0;
results.tstat = results.t0; % need this trick for printing below

fprintf(fid,'\n');
fprintf(fid,'Casetti X-Y Spatial Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',results.rbar);
fprintf(fid,'sige          = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base x-y estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% print expanded estimates here
beta = results.beta; % nobs x 2*(nvar-1) matrix
% redo variable names for expansion variables only
Vname = [];
 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = [];
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end; % end of if-else
end; % end of nflag issue

in2.rflag = 1;
in2.cnames = Vname;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(beta,in2);


else % case of distance expansion

nvar = results.nvar;

% make up generic names
Vname = [];
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=nvar+1:2*nvar-1
    tmp = ['d-variable ',num2str(i-nvar)];
    Vname = strvcat(Vname,tmp);
 end;

if (nflag == 1) % the user supplied variable names
Vname = [];
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 for i=1:nvar-1
    Vname = strvcat(Vname,['d-',vnames(i+2,:)]);
 end;
 end;
end; % end of nflag issue

bout = results.b0;
results.tstat = results.t0; % need this trick for printing below

fprintf(fid,'\n');
fprintf(fid,'Casetti Distance Spatial Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',results.rbar);
fprintf(fid,'sige          = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
fprintf(fid,'central obs   = %6d \n',results.ctr);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base centroid estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
Vname2 = strvcat('Variable',Vname);
in.cnames = cnames;
in.rnames = Vname2;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
fprintf(fid,'***************************************************************\n');
% print expanded estimates here
in2.cnames = Vname(2:nvar,:);
in2.rflag = 1;
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(results.beta,in2);


end; % end of casetti

case {'bcasetti'} % <=================== casetti expansion model

nvar = results.nvar;
nobs = results.nobs;
expansion = results.exp;

if expansion == 0 % case of x-y expansion
xvar = nvar-1;
yvar = nvar-1;

% special handling of vnames
% make up generic variable names
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,['variable',num2str(i)]);
end;

 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,vnames(i+1,:));
end;
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end; % end of if-else
end; % end of nflag issue

bout = mean(results.b0draw)';
stdb = std(results.b0draw)';
t0 = bout./stdb;
results.tstat = t0; % need this trick for printing below
tvar = length(bout);
y = results.y;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-tvar);
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-2*results.nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared


fprintf(fid,'\n');
fprintf(fid,'Bayesian Casetti X-Y Spatial Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',results.rbar);
fprintf(fid,'sige          = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
fprintf(fid,'prior r-value = %6d \n',results.rval);
fprintf(fid,'ndraws,nomit  = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs  = %9.4f   \n',results.time);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base x-y estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% print expanded estimates here
beta = results.beta; % nobs x 2*(nvar-1) matrix
% redo variable names for expansion variables only
Vname = [];
 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
Vname = [];
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = [];
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end;
end; % end of nflag issue


in2.rflag = 1;
in2.cnames = Vname;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(beta,in2);


else % case of distance expansion

nvar = results.nvar;

    % special handling of vnames
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;

 for i=nvar+1:2*nvar-1
    tmp = ['d-variable ',num2str(i-nvar)];
    Vname = strvcat(Vname,tmp);
 end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 error('Wrong # of variable names in prt_spat -- check vnames argument');
 end;
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 for i=1:nvar-1
    Vname = strvcat(Vname,['d-',vnames(i+2,:)]);
 end;

end; % end of nflag issue

bout = mean(results.b0draw)';
stdb = std(results.b0draw)';
t0 = bout./stdb;
results.tstat = t0; % need this trick for printing below
tvar = length(bout);
y = results.y;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-tvar);
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-2*results.nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

fprintf(fid,'\n');
fprintf(fid,'Bayesian Casetti Distance Spatial Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',results.rbar);
fprintf(fid,'sige          = %9.4f \n',results.sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
fprintf(fid,'central obs   = %6d \n',results.ctr);
fprintf(fid,'prior r-value = %6d \n',results.rval);
fprintf(fid,'ndraws,nomit  = %6d,%6d \n',results.ndraw,results.nomit);
fprintf(fid,'time in secs  = %9.4f   \n',results.time);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base centroid estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
Vname2 = strvcat('Variable',Vname);
in.cnames = cnames;
in.rnames = Vname2;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
fprintf(fid,'***************************************************************\n');
% print expanded estimates here
in2.cnames = Vname(2:nvar,:);
in2.rflag = 1;
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(results.beta,in2);


end; % end of bcasetti



case {'darp'} % <=================== casetti darp model

nvar = results.nvar;
nobs = results.nobs;
expansion = results.exp;

if expansion == 0 % case of x-y expansion
xvar = nvar-1;
yvar = nvar-1;

% special handling of vnames
% make up generic variable names
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,vnames(i+1,:));
end;
 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
for i=1:nvar
Vname = strvcat(Vname,vnames(i+1,:));
end;
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end; % end of if-else
end; % end of nflag issue

bout = results.b0;
results.tstat = results.t0; % need this trick for printing below

fprintf(fid,'\n');
fprintf(fid,'DARP X-Y Spatial Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared       = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared    = %9.4f \n',results.rbar);
fprintf(fid,'sige            = %9.4f \n',results.sige);
fprintf(fid,'gamma1,gamma2   = %9.4f,%9.4f \n',results.gamma(1),results.gamma(2));
fprintf(fid,'gamma1, prob    = %9.4f,%9.4f \n',results.chi(1),results.cprob(1));
fprintf(fid,'gamma2, prob    = %9.4f,%9.4f \n',results.chi(2),results.cprob(2));
fprintf(fid,'# of iterations = %6d \n',results.iter);
fprintf(fid,'log-likelihood  = %16.8g \n',results.lik);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base x-y estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% print expanded estimates here
beta = results.beta; % nobs x 2*(nvar-1) matrix
% redo variable names for expansion variables only
Vname = [];
 for i=1:xvar;
    tmp = ['x-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
 for i=1:yvar
    tmp = ['y-variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);    
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = [];
 for i=1:xvar
    Vname = strvcat(Vname,['x-',vnames(i+2,:)]);
 end;
 for i=1:yvar
    Vname = strvcat(Vname,['y-',vnames(i+2,:)]);
 end;
 end; % end of if-else
end; % end of nflag issue

in2.rflag = 1;
in2.cnames = Vname;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(beta,in2);


else % case of distance expansion

nvar = results.nvar;

    % special handling of vnames
if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;

 for i=nvar+1:2*nvar-1
    tmp = ['d-variable ',num2str(i-nvar)];
    Vname = strvcat(Vname,tmp);
 end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 error('Wrong # of variable names in prt_spat -- check vnames argument');
 end;
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 for i=1:nvar-1
    Vname = strvcat(Vname,['d-',vnames(i+2,:)]);
 end;

end; % end of nflag issue

bout = results.b0;
results.tstat = results.t0; % need this trick for printing below

fprintf(fid,'\n');
fprintf(fid,'DARP Distance Expansion Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'\n');
fprintf(fid,'R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',results.rbar);
fprintf(fid,'sige          = %9.4f \n',results.sige);
fprintf(fid,'gamma         = %9.4f \n',results.gamma(1));
fprintf(fid,'gamma, prob   = %9.4f,%9.4f \n',results.chi(1),results.cprob(1));
fprintf(fid,'# of iterations = %6d \n',results.iter);
fprintf(fid,'log-likelihood  = %16.8g \n',results.lik);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',results.nobs,nvar);
fprintf(fid,'central obs   = %6d \n',results.ctr);
if results.norm == 1
fprintf(fid,'Isotropic x-y normalization \n');
end;
fprintf(fid,'***************************************************************\n');
fprintf(fid,'Base centroid estimates \n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
Vname2 = strvcat('Variable',Vname);
in.cnames = cnames;
in.rnames = Vname2;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
fprintf(fid,'***************************************************************\n');
% print expanded estimates here
in2.cnames = Vname(2:nvar,:);
in2.rflag = 1;
fprintf(fid,'Expansion estimates \n');
in2.fid = fid;
in2.fmt = '%8.4f';
mprint(results.beta,in2);


end; % end of if else


otherwise
error('results structure not known by prt_spat function');
end;


