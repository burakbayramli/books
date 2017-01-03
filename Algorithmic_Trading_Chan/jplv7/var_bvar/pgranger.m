function pgranger(result,varargin)
% PURPOSE: prints VAR model Granger-causality results
%  --------------------------------------------------
% USAGE: pgranger(results,varargin);
%  where: results = a structure returned by vare()
%        varargin = a variable input list containing             
%         vnames  = an optional variable name vector
%         cutoff  = probability cutoff used when printing 
% usage: pgranger(result,0.05,vnames);                  
%    or: pgranger(result,0.05);
%    or: pgranger(result,vnames);
%    or: pgranger(result,vnames,0.01);  
%----------------------------------------------------               
%                 e.g. vnames = ['y1', VAR variables
%                                'y2']  
%                 e.g. cutoff = 0.05 would only print
%                      marginal probabilities < 0.05                
%---------------------------------------------------               
% NOTES: constant term is added automatically to vnames list
%       you need only enter VAR variable names plus deterministic                
%---------------------------------------------------  
% SEE ALSO: vare(), prt_var(), pftests()
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(result)
 error('pgranger requires a VAR model results structure');
end;

nflag = 0;
fid = 1;
cutoff = 1.0;

for i=1:length(varargin);
 if ischar(varargin{i})
  nflag = 1; % user-supplied vnames
  vnames = varargin{i};
      [namsiz nsize] = size(vnames);
      if namsiz < result(1).neqs
    error('pgranger vnames argument is wrong size');
      end;  
 else
  cutoff = varargin{i};
 end;
end;

% find neqs
neqs = result(1).neqs;

% recover probability matrix
prob = zeros(neqs,neqs);
for i=1:neqs;
 for j=1:neqs;
  if result(i).fprob(j) < cutoff;
  prob(i,j) = result(i).fprob(j);
  else
  prob(i,j) = NaN;
  end;
 end;
end;

% set up names
if nflag == 0
 rnames = 'Variable';
 cnames = [];
 for i=1:neqs
  rnames = strvcat(rnames,['var ',num2str(i)]);
  cnames = strvcat(cnames,['var ',num2str(i)]);
 end;
else
 rnames = 'Variable';
 cnames = [];
 for i=1:neqs
  rnames = strvcat(rnames,vnames(i,:));
  cnames = strvcat(cnames,vnames(i,:));
 end;
end;

in.rnames = rnames;
in.cnames = cnames;
in.fmt = '%10.2f';
in.fid = fid;

fprintf(fid,'\n ****** Granger Causality Probabilities *******\n');
mprint(prob,in);



