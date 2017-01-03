function prt_var(result,vnames,fid)
% PURPOSE: Prints vector autoregressive models output
%---------------------------------------------------
% USAGE:  prt_var(result,vnames,fid)        
%    where: 
%    results = a structure returned by:
%                      var,bvar,rvar,ecm,becm,recm
%         vnames  = optional vector of variable names
%             fid = (optional) file-id for printing to a file
%                   (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = strvcat('y1','y2','x1','x2');
%                 e.g. fid = fopen('var.out','wr');
%---------------------------------------------------               
% NOTE: - constant term is added automatically to vnames list
%         you need only enter VAR variable names plus deterministic    
%       - you may use prt_var(results,[],fid) to print
%         output to a file with no vnames                   
%---------------------------------------------------               
% SEE ALSO: prt, plt
%---------------------------------------------------

if nargin < 1; error('wrong # of arguments to prt_var'); end;
if nargin > 3; error('wrong # of arguments to prt_var'); end;

if ~isstruct(result);
 error('prt_var requires a VAR model results structure');
end;

nflag = 0;
if nargin == 1; fid = 1;            end;
if nargin == 2; nflag = 1; fid = 1; end;
if nargin == 3; 
[vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
end;

% find nobs, nvar (used throughout)
nobs = result(1).nobs;
neqs = result(1).neqs;
nlag = result(1).nlag;
k    = result(1).nvar;
nx = k - neqs*nlag;

switch result(1).meth

case {'ar','vare','bvar','rvar'} % <====== construct variable names for these models

% set up VAR, BVAR variable names used throughout
% --------------------------------------------------------

if nflag == 0 % # no variable names supplied
Vname = [];
lnames = [];

l=1;
for i=1:neqs;
for m=1:nlag;
    Vname{l}  = str2mat(['variable ',num2str(i)]);
    lnames{l} = str2mat(['  lag',num2str(m)]);
l = l+1;
end;
end;
for i=l:k
if i < k
 Vname{i} = str2mat(['dvariable ',num2str(i-l+1)]);
   lnames{i} = '      ';
else
 Vname{i} = 'constant ';
   lnames{i} = '      ';      
end;
end;

end; % end of if nflag == 0

% --------------------------------------------------------
% # the user supplies variable names
% --------------------------------------------------------
if (nflag == 1)
Vname = [];
lnames = [];

[namesize namewidth] = size(vnames);
if namesize ~= neqs+nx-1
 error('wrong # of vnames in prt_var');
end;

l=1;
for i=1:neqs;
for m=1:nlag;
    Vname{l} = vnames(i,:);
    lnames{l} = str2mat(['  lag',num2str(m)]);
l = l+1;
end;
end;

cnt = 1;
for i=l:k
  if i < k 
   Vname{i} = vnames(neqs+cnt,:);
   lnames{i} = '      '; 
   cnt = cnt+1;
  else
   Vname{i} = 'constant ';
   lnames{i} = '      ';      
  end;
end;

end; % end of if nflag == 1

case {'ecm','becm','recm'} % <====== construct variable names for these models

% --------------------------------------------------------
% # no variable names supplied
% --------------------------------------------------------
if nflag == 0
Vname = [];
lnames = [];

l=1;
for i=1:neqs;
for m=1:nlag;
    Vname{l}  = str2mat(['variable ',num2str(i)]);
    lnames{l} = str2mat(['  lag',num2str(m)]);
l = l+1;
end;
end;

for i=l:k
if i < k
 Vname{i} = str2mat(['ec term ',num2str(i-l+1)]);
   lnames{i} = '      ';
else
 Vname{i} = 'constant ';
   lnames{i} = '      ';      
end;
end;

end; % end of if nflag == 0

% --------------------------------------------------------
% # the user supplies variable names
% --------------------------------------------------------
if (nflag == 1)
Vname = [];
lnames = [];

l=1;
for i=1:neqs;
for m=1:nlag;
    Vname{l} = vnames(i,:);
    lnames{l} = str2mat(['  lag',num2str(m)]);
l = l+1;
end;
end;

cnt = 1;
for i=l:k
  if i < k 
   Vname{i} = str2mat(['ec term ',vnames(cnt,:)]);
   lnames{i} = '      '; 
   cnt = cnt+1;
  else
   Vname{i} = 'constant ';
   lnames{i} = '      ';      
  end;
end;

end; % end of if nflag == 1

end; % end of switch


% column headers used throughout
vstring = 'Variable';
lstring = 'Lag';
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';


switch result(1).meth

case {'vare'} % <=================== var-model

fprintf(fid,'\n ***** Vector Autoregressive Model ***** \n');

for j=1:neqs;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3d \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',result(j).rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result(j).rbar);
fprintf(fid,'sige          = %9.4f \n',result(j).sige);
fprintf(fid,'Q-statistic   = %9.4f \n',result(j).boxq);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,k);
fprintf(fid,'******************************************************************\n');

% pull out equation j results
bhat  = result(j).beta;
tstat = result(j).tstat;
tprob = result(j).tprob;
tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:k
    tmpn{i} = [Vname{i} lnames{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

% print out Granger-Causality test results
fstring = 'F-value';
fpstring = 'Probability';

gin.cnames = strvcat(fstring,fpstring);
rnames = vstring;
tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Variable ' num2str(i)]);
    end;
tmp(i,:) = [result(j).ftest(i) result(j).fprob(i)];
end;
gin.rnames = rnames;
gin.fmt = '%16.6f';
gin.fid = fid;
fprintf(fid,' ****** Granger Causality Tests *******\n');

mprint(tmp,gin); 
fprintf(fid,'\n');
end; % end of for j loop over all equations

% ==================== end of case var

case {'bvar','rvar'} % <=================== bvar/rvar -model

fprintf(fid,'\n ***** Bayesian Vector Autoregressive Model ***** \n');

if strcmp(result(1).meth,'bvar')
fprintf(fid,'\n *****    Minnesota type Prior         ***** \n');
fprintf(fid,'\nPRIOR hyperparameters \n');
fprintf(fid,'tightness = %8.2f \n',result(1).tight);
fprintf(fid,'decay     = %8.2f \n',result(1).decay);

[n1 n2] = size(result(1).weight);
if n1 > 1 % print out weight matrix
rnames = 'Variable';
cnames = [];
    tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    cnames = strvcat(cnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Var ' num2str(i)]);
    cnames = strvcat(cnames,['Var ' num2str(i)]);
    end;
end;
tmp = result(1).weight;
in.cnames = cnames;
in.rnames = rnames;
in.fmt = '%8.2f';
in.fid = fid;
fprintf(fid,'Weights matrix \n');
mprint(tmp,in);

else % print out scalar weight 
 fprintf(fid,'Symmetric weights based on ');
 fprintf(fid,'%8.2f \n\n',result(1).weight);
end;

else
fprintf(fid,'\n *****    Random-Walk Averaging Prior         ***** \n');
fprintf(fid,'\nPRIOR hyperparameters \n');
fprintf(fid,'sig   = %8.2f \n',result(1).sig);
fprintf(fid,'tau   = %8.2f \n',result(1).tau);
fprintf(fid,'theta = %8.2f \n',result(1).theta);
fprintf(fid,'Weight matrix = \n');

rnames = 'Variable';
cnames = [];
    tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    cnames = strvcat(cnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Var ' num2str(i)]);
    cnames = strvcat(cnames,['Var ' num2str(i)]);
    end;
end;
tmp = result(1).weight;
in.cnames = cnames;
in.rnames = rnames;
in.fmt = '%8.2f';
in.fid = fid;
mprint(tmp,in);
end;

for j=1:neqs;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3d \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',result(j).rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result(j).rbar);
fprintf(fid,'sige          = %9.4f \n',result(j).sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,k);
fprintf(fid,'******************************************************************\n');

bhat  = result(j).beta;
tstat = result(j).tstat;
if strcmp(result(1).meth,'bvar')
tprob = result(j).tprob;
else
tprob = tdis_prb(tstat,nobs);
end;

tmp = [bhat tstat tprob];

% print out results
rnames = vstring;
for i=1:k
    tmpn{i} = [Vname{i} lnames{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.cnames = strvcat(bstring,tstring,pstring);
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

fprintf(fid,'\n');
end; % end of for j loop over all equations

% ==================== end of case bvar

case {'ecm'} % <===================ecm-model

fprintf(fid,'\n ***** Error Correction Model ***** \n');

for j=1:neqs;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3d \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',result(j).rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result(j).rbar);
fprintf(fid,'sige          = %9.4f \n',result(j).sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,k);
fprintf(fid,'******************************************************************\n');

bhat  = result(j).beta;
tstat = result(j).tstat;
tprob = result(j).tprob;



tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:k
    tmpn{i} = [Vname{i} lnames{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

% print out Granger-Causality test results
fstring = 'F-value';
fpstring = 'Probability';

in.cnames = strvcat(fstring,fpstring);
rnames = vstring;
tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Variable ' num2str(i)]);
    end;
tmp(i,:) = [result(j).ftest(i) result(j).fprob(i)];
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
fprintf(fid,' ****** Granger Causality Tests *******\n');
mprint(tmp,in); 

end; % end of for j loop over all equations

% print out johansen co-integration test results

nobs = length(result(1).y);
ylevel = zeros(nobs,neqs);

for j=1:neqs;
 ylevel(:,j) = result(j).y;
end;

cres = johansen(ylevel,0,nlag);

if nflag == 1
 prt_coint(cres,vnames,fid);
else
 prt_coint(cres,[],fid);
end;

% end of ecm case

case {'becm','recm'} % <=================== becm, recm-model

fprintf(fid,'\n ***** Bayesian Error Correction Model ***** \n');

if strcmp(result(1).meth,'becm')
fprintf(fid,'\n *****    Minnesota type Prior         ***** \n');
fprintf(fid,'\nPRIOR hyperparameters \n');
fprintf(fid,'tightness = %8.2f \n',result(1).tight);
fprintf(fid,'decay     = %8.2f \n',result(1).decay);

[n1 n2] = size(result(1).weight);
if n1 > 1 % print out weight matrix
rnames = 'Variable';
cnames = [];
    tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    cnames = strvcat(cnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Var ' num2str(i)]);
    cnames = strvcat(cnames,['Var ' num2str(i)]);
    end;
end;
tmp = result(1).weight;
in.rnames = rnames;
in.cnames = cnames;
in.fmt = '%8.2f';
in.fid = fid;
fprintf(fid,'Weights matrix \n');
mprint(tmp,in);

else
 fprintf(fid,'Symmetric weights based on ');
 fprintf(fid,'%8.2f \n\n',result(1).weight);
end;

else

fprintf(fid,'\n *****    Random-Walk Averaging Prior         ***** \n');
fprintf(fid,'\nPRIOR hyperparameters \n');
fprintf(fid,'sig   = %8.2f \n',result(1).sig);
fprintf(fid,'tau   = %8.2f \n',result(1).tau);
fprintf(fid,'theta = %8.2f \n',result(1).theta);
fprintf(fid,'Weight matrix = \n');

rnames = 'Variable';
cnames = [];
    tmp = [];
for i=1:neqs
    if nflag == 1
    rnames = strvcat(rnames,vnames(i,:));
    cnames = strvcat(cnames,vnames(i,:));
    else
    rnames = strvcat(rnames,['Var ' num2str(i)]);
    cnames = strvcat(cnames,['Var ' num2str(i)]);
    end;
end;
tmp = result(1).weight;
in.rnames = rnames;
in.cnames = cnames;
fmt = '%8.2f';
in.fid = fid;
mprint(tmp,in);
end;

for j=1:neqs;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3d \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',result(j).rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result(j).rbar);
fprintf(fid,'sige          = %9.4f \n',result(j).sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,k);
fprintf(fid,'******************************************************************\n');

bhat  = result(j).beta;
tstat = result(j).tstat;
if strcmp(result(1).meth,'becm')
tprob = result(j).tprob;
else
tprob = tdis_prb(tstat,nobs);
end;


tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:k
    tmpn{i} = [Vname{i} lnames{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

fprintf(fid,'\n');
end; % end of for j loop over all equations

% print out johansen co-integration test results
nobs = length(result(1).y);
ylevel = zeros(nobs,neqs);

for j=1:neqs;
 ylevel(:,j) = result(j).y;
end;

cres = johansen(ylevel,0,nlag);

if nflag == 1
 prt_coint(cres,vnames,fid);
else
 prt_coint(cres,[],fid);
end;

% end of becm/recm case

otherwise
error('results structure unknown to prt_var');
end;

  
  
