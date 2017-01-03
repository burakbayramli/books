function bkw(x,vnames,fmt)
% PURPOSE: computes and prints BKW collinearity diagnostics
%          variance-decomposition proportions matrix
%---------------------------------------------------
% USAGE: bkw(x,vnames,fmt)
% where:       x = independent variable matrix (from a regression model)
%         vnames = (optional) variable name vector    
%            fmt = (optional) format string, e.g., '%12.6f' or '%12d' 
%                  default = %10.2f   
%---------------------------------------------------
% NOTE: you can use either x-variable names or an ols
%       vnames argument containing y-variable + x-variable names
% e.g. vnames = strvcat('y','x1','x2') or
%      vnames = strvcat('x1','x2') 
%---------------------------------------------------
% RETURNS:
%        nothing, just prints the table out
% --------------------------------------------------
% SEE ALSO: dfbeta, rdiag, diagnose
%---------------------------------------------------
% REFERENCES: Belsley, Kuh, Welsch, 1980 Regression Diagnostics
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


[nobs nvar] = size(x);
fid = 1;
% error checking on inputs
if nargin == 3
   nflag = 1;
elseif nargin == 2
 nflag = 1;
   fmt = '%10.2f';
elseif nargin == 1
   nflag = 0;
   fmt = '%10.2f';
else
error('Wrong # of arguments to bkw');   
end;

[u d v] = svd(x,0);

lamda = diag(d(1:nvar,1:nvar));
lamda2 = lamda.*lamda;
v = v.*v;

phi = zeros(nvar,nvar);
for i=1:nvar;
phi(i,:) = v(i,:)./lamda2';
end;


pi = zeros(nvar,nvar);
for i=1:nvar;
phik = sum(phi(i,:));
pi(i,:) = phi(i,:)/phik;
end;

% BUG fix suggested by 
% John P. Burkett <burkett@uriacc.uri.edu
lmax = lamda(1);
lmaxvec = lmax*ones(nvar,1);
lout = lmaxvec./lamda;


out = pi';

% make up some generic names
  Vname = [];
  for i=1:nvar
   if i < 10
     snames = 'var ';
     name = [snames num2str(i)];
     Vname = [Vname
               name];
     else
     snames = 'var';
     name = [snames num2str(i)];
     Vname = [Vname
               name];
     end;
    end;
if nflag == 1 
 [namsiz nsize] = size(vnames); % error check vnames argument
  if namsiz == nvar+1
    Vname = [];
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
    end;
  elseif namsiz == nvar
    Vname = [];
    for i=1:nvar
    Vname = strvcat(Vname,vnames(i,:));
    end;
  else
 fprintf(fid,'Wrong # of variable names in bkw -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
  end; % end of if-elseif,else
end; % end of if

rnames = strvcat('K(x)',num2str(round(lout)));
in.fmt = fmt;
in.rnames = rnames;
in.cnames = Vname;
fprintf('\n Belsley, Kuh, Welsch Variance-decomposition \n');
mprint(out,in);


