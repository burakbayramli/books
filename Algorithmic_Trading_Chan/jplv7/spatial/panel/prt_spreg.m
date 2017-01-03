function prt_spreg(results,vnames,fid)
% PURPOSE: Prints output using spatial regression results structures
%---------------------------------------------------
% USAGE: prt_sp(results,vnames,fid)
% Where: results = a structure returned by a spatial panel regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_sp(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial panel regression results
% --------------------------------------------------

if ~isstruct(results)
 error('prt_sp requires structure argument');
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
 error('Wrong # of arguments to prt_sp');
end;

nvar = results.nvar;
nobs = results.nobs;

% handling of vnames
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_sp -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 end; % end of if-else
end; % end of nflag issue

fprintf(fid,'\n');

switch results.meth
case {'semreg'}
fprintf(fid,'Pooled model with spatial error autocorrelation and two regimes, no fixed effects \n');
case {'semregsfe'}
fprintf(fid,'Pooled model with spatial error autocorrelation, spatial fixed effects and two regimes\n');
case {'semregtfe'}
fprintf(fid,'Pooled model with spatial error autocorrelation, time period fixed effects and two regimes \n');
case {'semregstfe'}
fprintf(fid,'Pooled model with spatial error autocorrelation, spatial and time period fixed effects, and two regimes \n');
case {'sarreg'}
fprintf(fid,'Pooled model with spatially lagged dependent variable and two regimes, no fixed effects \n');
case {'sarregsfe'}
fprintf(fid,'Pooled model with spatially lagged dependent variable, spatial fixed effects and two regimes\n');
case {'sarregtfe'}
fprintf(fid,'Pooled model with spatially lagged dependent variable, time period fixed effects and two regimes\n');
case {'sarregstfe'}
fprintf(fid,'Pooled model with spatially lagged dependent variable, spatial and time period fixed effects, and two regimes \n');
otherwise
error('results structure not known by prt_sp function');
end % switch statement

switch results.meth

case {'semreg','semregsfe','semregtfe','semregstfe'} % <=================== spatial panel error models

% add spatial rho parameter name
    Vname = strvcat(Vname,'regime 1','regime 2');
    
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared       = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared    = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2         = %9.4f   \n',results.sige);
fprintf(fid,'log-likelihood  = %16.8g  \n',results.lik);
fprintf(fid,'Nobs,Nvar,TNvar = %6d,%6d,%6d  \n',results.nobs,results.nvar,results.tnvar);
fprintf(fid,'# iterations    = %6d     \n',results.iter);
fprintf(fid,'min and max rho = %9.4f,%9.4f \n',results.rmin,results.rmax);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho];
        
% <=================== end of sem case

case {'sarreg','sarregsfe','sarregtfe','sarregstfe'} % <=================== spatial panel autoregressive models

% add spatial rho parameter name
    Vname = strvcat(Vname,'regime 1','regime 2');
    
if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared         = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared      = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f \n',results.sige);
fprintf(fid,'Nobs,Nvar,TNvar    = %6d,%6d,%6d  \n',results.nobs,results.nvar,results.tnvar);
fprintf(fid,'log-likelihood     = %16.8g \n',results.lik);
fprintf(fid,'# of iterations    = %6d   \n',results.iter);
fprintf(fid,'min and max rho    = %9.4f,%9.4f \n',results.rmin,results.rmax);
% print timing information
fprintf(fid,'total time in secs = %9.4f \n',results.time);
fprintf(fid,'***************************************************************\n');

bout = [results.beta
        results.rho];
        
% <=================== end of sar case

otherwise
error('results structure not known by prt_sp function');
end;

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(results.tstat); % find asymptotic z (normal) probabilities, function of LeSage
tmp = [bout results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); %function of LeSage

fprintf(fid,'Difference rho1-rho2 and T-value = %9.4f,%9.4f \n',results.dif,results.tdif);