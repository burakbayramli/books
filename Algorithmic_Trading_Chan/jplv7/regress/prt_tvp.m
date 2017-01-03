function prt_tvp(results,vnames,fid)
% PURPOSE: Prints output using tvp() regression results structures
%---------------------------------------------------
% USAGE: prt_tvp(results,vnames,fid)
% Where: results = a structure returned by a regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_tvp(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the regression results
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
 error('prt_tvp requires structure argument');
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
 error('Wrong # of arguments to prt_tvp');
end;

nobs = results.nobs;
nvar = results.nvar;

% make up some generic variable names
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_tvp -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 end; % end of if-else
end; % end of nflag issue

switch results.meth

case {'tvp'} % <=================== time-varying parameter regression


fprintf(fid,'\n');
if results.delta == 0
fprintf(fid,'Time-Varying Parameter Estimates \n');
else
 fprintf(fid,'Time-Varying Parameter Estimates -- Zellner g-prior \n');
end;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
if results.delta == 0 % case of full model, no g-prior
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results.rbar);
fprintf(fid,'sigma          = %9.4f \n',results.sige);
fprintf(fid,'std sigma      = %9.4f \n',results.stds);
fprintf(fid,'sigma t-stat   = %9.4f \n',results.sige/results.stds);
else % case of g-prior
fprintf(fid,'sigma,delta          = %9.4f,%9.4f \n',results.sige,results.delta);
fprintf(fid,'std sigma,delta      = %9.4f,%9.4f \n',results.stds,results.stdd);
t1 = results.sige/results.stds;
t2 = results.delta/results.stdd;
fprintf(fid,'sigma,delta t-stat   = %9.4f,%9.4f \n',t1,t2);
end;
fprintf(fid,'log-likelihood = %9.4f \n',results.like);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'Starting obs   = %6d \n',results.start);
fprintf(fid,'Iterations     = %6d \n',results.iter);
fprintf(fid,'Time in secs   = %9.1f \n',results.time);
fprintf(fid,'***************************************************************\n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat(2:nvar+1,1),nobs);      % find t-stat probabilities
tmp = [results.sigb results.stdb results.tstat(2:nvar+1,1) tout];  
% tmp = matrix to be printed
% column labels for printing results
bstring = 'Coeffs (sigb)';
sstring = 'std dev'; 
tstring = 't-statistic'; 
pstring = 't-probability';
cnames = strvcat(bstring,sstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% now print time-varying paramater estimates ?



% <=================== end of time-varying parameter model case

case {'tvp_garch'} % <=================== tvp model with garch(1,1) errors

fprintf(fid,'\n');
fprintf(fid,'Time-Varying Parameter Estimates with garch(1,1) errors \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results.rbar);
fprintf(fid,'log-likelihood = %9.4f \n',results.like);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'Starting obs   = %6d \n',results.start);
fprintf(fid,'Iterations     = %6d \n',results.iter);
fprintf(fid,'Time in secs   = %9.1f \n',results.time);
fprintf(fid,'***************************************************************\n');

% <=================== end of tvp with garch(1,1) errors case
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs);      % find t-stat probabilities
temp = [results.sigb
        results.ahat];
tmp = [temp results.stdhat results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coeffs (sigb)';
sstring = 'std dev';
tstring = 't-statistic'; 
pstring = 't-probability';
cnames = strvcat(bstring,sstring,tstring,pstring);
in.cnames = cnames;
% tack a0,a1,a2 onto Vname string vector
Vname = strvcat(Vname,'garch a0','sig(t-1)','e(t-1)^2');
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

case {'tvp_markov'} % <=================== time-varying parameter with Markov switching regression

% REDO variable names here
% make up some generic variable names
Vname = [];
 for i=1:nvar
    tmp = ['variance ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_tvp -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = [];
 for i=1:nvar
    tname = strcat('Var(',vnames(i+1,:));
    tname = strcat(tname,')');
    Vname = strvcat(Vname,tname);
 end;
 end; % end of if-else
end; % end of nflag issue


fprintf(fid,'\n');
fprintf(fid,'Time-Varying Parameter with Markov switching variances Estimates \n');
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results.rbar);
fprintf(fid,'log-likelihood = %9.4f \n',results.like);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'Starting obs   = %6d \n',results.start);
fprintf(fid,'Iterations     = %6d \n',results.iter);
fprintf(fid,'Time in secs   = %9.1f \n',results.time);
fprintf(fid,'***************************************************************\n');
% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs);      % find t-stat probabilities
tmp = [results.parm results.stdhat results.tstat tout];  
% tmp = matrix to be printed
% column labels for printing results
bstring = 'Coeffs';
sstring = 'std dev'; 
tstring = 't-statistic'; 
pstring = 't-probability';
cnames = strvcat(bstring,sstring,tstring,pstring);
in.cnames = cnames;
rnames = 'Variable';
rnames = strvcat(rnames,'p= Pr(St=1 | St-1=1)','q= Pr(St=0 | St-1=0)');
rnames = strvcat(rnames,Vname);
rnames = strvcat(rnames,'sige1','sige2');
in.rnames = rnames;

in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);

% now print time-varying paramater estimates ?



otherwise
error('results structure not known by prt_tvp function');
end;

