function prt_coint(result,vnames,fid)
% PURPOSE: Prints output from co-integration tests
%          adf,cadf,johansen,phillips
%---------------------------------------------------
% USAGE: prt_coint(results,vnames,fid,eflag)
% Where: results = a structure returned by a co-integration test
%        vnames  = an optional vector of variable names
%        fid     = file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];
%                 e.g. fid = fopen('coint.out','wr');
% --------------------------------------------------
% RETURNS:
%        nothing, just prints the co-integration test results
% --------------------------------------------------
% NOTES: you may use prt_coint(results,[],fid) to print
%        output to a file with no vnames
% --------------------------------------------------    
% SEE ALSO: prt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% revised 10/10/2000
% to fix error in mprint fid
% Thanks to JChapman@bank-banque-canada.ca
% for pointing out this error

 
% if nargin < 1; error('wrong # of arguments to prt_coint'); end;
if nargin > 3; error('wrong # of arguments to prt_coint'); end;

nflag = 0;
if nargin == 1; fid = 1;            end;
if nargin == 2; nflag = 1; fid = 1; end;
if nargin == 3; 
[vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
end;

% --------------------------------------------------------


switch result.meth
 
case {'johansen'} % <=================== johansen tests
 
 
[nvar junk] = size(result.eig);

%  make some up variable names
Vname = [];
% recover variable order index
index = result.ind;

for i=1:nvar
    Vname(i,:) = str2mat(['variable   ',num2str(index(i))]);
end;

if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar
 fprintf(fid,'Wrong # of variable names in prt_coint -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
% recover variable order index
index = result.ind;   
Vname = [];
 for i=1:nvar
    Vname = strvcat(Vname,vnames(index(i),:));
 end;
 end; % end of if-else
end; % end of nflag issue


rname = 'NULL:';
for i=1:nvar
tmp = ['r <= ',num2str(i-1)];
tmp = [tmp,'   '];
tmp = [tmp,Vname(i,:)];
rname = strvcat(rname,tmp);
end;
cname = strvcat('Trace Statistic','Crit 90%','Crit 95%','Crit 99%');
prtmat = [result.lr1 result.cvt];
in.cnames = cname;
in.rnames = rname;
in.fmt = '%16.3f';
in.fid = fid;

fprintf(fid,'\n Johansen MLE estimates \n');
mprint(prtmat,in);
cname = strvcat(' Eigen Statistic','Crit 90%','Crit 95%','Crit 99%');
in.cnames = cname;
prtmat = [result.lr2 result.cvm];
mprint(prtmat,in);


% end of Johansen
  
case {'adf'} % <=================== adf tests

if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];

for i=1:1
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];

[tst_n nsize] = size(vnames);

nmax = min(nsize,16); % truncate vnames to 16-characters

for i=1:1
Vname{i} = vnames(i,1:nmax);
end;

end; % end of nflag issue
 

  fprintf(fid,'\nAugmented DF test for unit root variable: %30s \n',Vname{1});
  
      in.cnames = strvcat('ADF t-statistic','# of lags','AR(1) estimate');
      in.fmt = strvcat('16.6f','%10d','%16.6f');
      tmp = [result.adf result.nlag result.alpha];
      mprint(tmp,in);
 
  in2.cnames = strvcat('1% Crit Value','5% Crit Value','10% Crit Value');
  in2.fmt = '%16.3f';
  in2.fid = fid;

      mprint(result.crit(1:3)',in2);
  
case {'cadf'}


if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];

for i=1:2
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];

[tst_n nsize] = size(vnames);

if tst_n ~= 2
error('Wrong # of variable names in prt_coint -- check vnames argument');
end;

nmax = min(nsize,16); % truncate vnames to 16-characters

for i=1:2
Vname{i} = vnames(i,1:nmax);
end;

end; % end of nflag issue
 
  fprintf(fid,'\n Augmented DF test for co-integration');
      fprintf(fid,' variables: %30s  \n',strjust([Vname{1},',',Vname{2}]));
      
       in.cnames = strvcat('CADF t-statistic','# of lags','AR(1) estimate');
      in.fmt = strvcat('16.8f','%10d','%16.6f');
      tmp = [result.adf result.nlag result.alpha];
      mprint(tmp,in);
 
  in2.cnames = strvcat('1% Crit Value','5% Crit Value','10% Crit Value');
  in2.fmt = '%16.3f';
  in2.fid = fid;
  mprint(result.crit(1:3)',in2);
      
  % end of cadf  

case {'phillips'}


if ( nflag == 0) %  no variable names supplied, make some up
Vname = [];

for i=1:2
    Vname{i} = str2mat(['variable   ',num2str(i)]);
end;

elseif (nflag == 1) % the user supplied variable names
Vname = [];

[tst_n nsize] = size(vnames);

if tst_n ~= 2
error('Wrong # of variable names in prt_coint -- check vnames argument');
end;

nmax = min(nsize,16); % truncate vnames to 16-characters

for i=1:2
Vname{i} = vnames(i,1:nmax);
end;

end; % end of nflag issue
 
  fprintf(fid,'\n Phillips-Peron test for co-integration');
      fprintf(fid,' variables: %30s  \n',strjust([Vname{1},',',Vname{2}]));
      
       in.cnames = strvcat('Phillips-Peron statistic','# of lags','AR(1) estimate');
      in.fmt = strvcat('16.8f','%10d','%16.6f');
      tmp = [result.pstat result.nlag result.alpha];
      mprint(tmp,in);
 
  in2.cnames = strvcat('1% Crit Value','5% Crit Value','10% Crit Value');
  in2.fmt = '%16.3f';
  in2.fid = fid;
      mprint(result.crit(1:3)',in2);
      
  % end of phillips  
    
  
   otherwise
error('results structure not known by prt_coint function');

end;
