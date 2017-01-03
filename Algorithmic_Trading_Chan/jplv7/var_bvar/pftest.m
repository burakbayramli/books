function pftest(result,vnames,fid);
% PURPOSE: prints VAR model ftests
%  --------------------------------------------------
% USAGE: pftest(results,vnames);
%  where: results = a structure returned by vare()
%         vnames  = an optional variable name vector
%             fid = file-id for printing results to a file
%                   (defaults to the MATLAB command window)               
%           e.g. vnames = ['y1', VAR variables
%                          'y2']  
%           e.g. fid = fopen('ftest.out','wr');
%----------------------------------------------------                              
% SEE ALSO: vare() prt_var(), pgranger()
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin > 3
 error('wrong # of arguments to pftest');
end;

nflag = 0;
if nargin == 1;
nflag = 0;
fid = 1;
elseif nargin == 2; 
 nflag = 1; 
   fid = 1; 
  [namsiz junk] = size(vnames);
   if namsiz < result(1).neqs
 error('pftest vnames argument is wrong size');
   end; 
end;

if ~isstruct(result)
 error('pftest requires a var model results structure');
end;

% find neqs
neqs = result(1).neqs;

fprintf(fid,'\n ****** Granger Causality Tests *******\n');

for j=1:neqs
% recover causality test results sequentially
% and print
gresults = zeros(neqs,2);

for i=1:neqs;
 gresults(i,1) = result(j).ftest(i);
 gresults(i,2) = result(j).fprob(i);
end;

% set up names
if nflag == 0
 rnames = ['Equation ',num2str(j)];
 cnames = strvcat('F-value','F-probability');
 for i=1:neqs
  rnames = strvcat(rnames,['var ',num2str(i)]);
 end;
else
 rnames = ['Equation  ',vnames(j,:)];
 cnames = strvcat('F-value','F-probability');
 for i=1:neqs
  rnames = strvcat(rnames,vnames(i,:));
 end;
end;

in.rnames = rnames;
in.cnames = cnames;
in.fmt = '%16.4f';
in.fid = fid;

mprint(gresults,in);

end; % end of loop over equations

