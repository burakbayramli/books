function prt_eqs(results,vnames,fid)
% PURPOSE: Prints output from mutliple equation regressions
%          (thsls,sur)
%---------------------------------------------------
% USAGE: prt_eqs(results,vnames,fid)
% Where: results = a structure returned by:
%                  thsls,sur
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];
%                 e.g. fid = fopen('ols.out','wr');
% --------------------------------------------------
% SEE ALSO: prt, plt, plt_reg(results)
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin < 1; error('wrong # of arguments to prt_eqs'); end;
if nargin > 3; error('wrong # of arguments to prt_eqs'); end;

nflag = 0;
if nargin == 1; fid = 1;            end;
if nargin == 2; nflag = 1; fid = 1; end;
if nargin == 3; nflag = 1;          end;

% --------------------------------------------------------

% error check for a structure input
if ~isstruct(results)
 error('prt_eqs requires a regression results structure');
end;


nobs = results(1).nobs;
neqs = results(1).neqs;

k=0;
for i=1:neqs;
k = k + results(i).nvar;
end;

cnt = 1;
cntn = 1;
eqname = [];
for neqn=1:neqs;

nvar = results(neqn).nvar;

% Make up generic variable names
 Vname = [];
 for i=1:nvar
        Vname{i} = str2mat(['variable   ',num2str(i)]);
 end;
if (nflag == 1) % the user supplied variable names
  [tst_n junk] = size(vnames);
  if tst_n ~= k+neqs;
   fprintf(fid,'Wrong # of variable names in prt_eqs -- check vnames argument \n');
   fprintf(fid,'will use generic variable names \n');
   nflag = 0;
  else,
   Vname = [];
    for i=1:nvar
    Vname{i} = vnames(cntn+i,:);
    end;
  end;
end; % end of nflag issue

switch results(1).meth

case {'thsls'} % <=================== thsls regressions


fprintf(fid,'\n');

fprintf(fid,'Three Stage Least-squares Estimates -- Equation %3d \n',neqn);

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(cntn,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results(neqn).rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results(neqn).rbar);
fprintf(fid,'sigma^2        = %9.4f \n',results(neqn).sige);
fprintf(fid,'Durbin-Watson  = %9.4f \n',results(neqn).dw);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results(1).nobs,results(neqn).nvar);
fprintf(fid,'***************************************************************\n');

% -------- end of thsls case

case {'sur'} % <=================== sur regressions


fprintf(fid,'\n');

fprintf(fid,'Seemingly Unrelated Regression -- Equation %3d \n',neqn);

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(cntn,:));
end;
fprintf(fid,'System R-sqr   = %9.4f \n',results(neqs).srsqr);
fprintf(fid,'R-squared      = %9.4f \n',results(neqn).rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results(neqn).rbar);
fprintf(fid,'sigma^2        = %9.4f \n',results(neqn).sige);
fprintf(fid,'Durbin-Watson  = %9.4f \n',results(neqn).dw);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results(1).nobs,results(neqn).nvar);
fprintf(fid,'***************************************************************\n');

% -------- end of sur case

otherwise
error('structure unknown to prt_eqs');
end;


tmp = [results(neqn).beta results(neqn).tstat results(neqn).tprob];
% print out results

vstring = 'Variable';
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';

cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
rnames = strvcat(rnames,Vname{i});
end;
fmt = '%16.6f';
in.fid = fid;
in.fmt = fmt;
in.cnames = cnames;
in.rnames = rnames;

mprint(tmp,in);
if nflag == 1
eqname{neqn} = vnames(cntn,:);
end;
cntn = cntn+nvar+1;
cnt = cnt+1;

end; % end of loop over equations

% print cross-equation correlation structure

sig =   results(1).sigma;

fprintf(fid,'Cross-equation sig(i,j) estimates \n');
cnames = [];
for i=1:neqs;
 if nflag == 0
 cnames = [cnames
          'eq ' num2str(i)];
 else
 cnames = [cnames
           eqname{i}];
 end;
end;
cnames = strvcat(cnames);
fmt = '%9.4f';
rnames = strvcat('equation',cnames);          
ins.fmt = fmt;
ins.cnames = cnames;
ins.rnames = rnames;
ins.fid = fid;

mprint(sig,ins);
fprintf(fid,'\n');

ccor = results(1).ccor;
ins.fmt = '%9.4f';

fprintf(fid,'Cross-equation correlations \n');

mprint(ccor,ins);



