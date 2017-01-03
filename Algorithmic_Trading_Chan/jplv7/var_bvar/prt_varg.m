function prt_varg(result,vnames,fid)
% PURPOSE: Prints vector autoregression output
%          from: bvar_g,rvar_g,becm_g,recm_g models
%---------------------------------------------------
% USAGE:     prt_varg(result,vnames,fid)        
%     where: results = a Gibbs var structure 
%               vnames  = optional vector of variable names
%                   fid = (optional) file-id for printing results to a file
%                         (defaults to the MATLAB command window)
%---------------------------------------------------               
%                 e.g. vnames = ['y1   ', VAR variables
%                                'y2   ',  
%                                'x1   ', deterministic variables       
%                                'x2'];
%                 e.g. fid = fopen('var.out','wr');
%---------------------------------------------------               
% NOTE: - constant term is added automatically to vnames list
%         you need only enter VAR variable names plus deterministic    
%       - you may use prt_varg(results,[],fid) to print
%         output to a file with no vnames                   
%---------------------------------------------------               
% SEE ALSO: prt
%---------------------------------------------------

if nargin < 1; error('wrong # of arguments to prt_varg'); end;
if nargin > 3; error('wrong # of arguments to prt_varg'); end;

if ~isstruct(result);
 error('prt_varg requires a Gibbs VAR model results structure');
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

case {'bvar_g','rvar_g'} % <====== construct variable names for these models

% set up BVAR, RVAR variable names used throughout
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
 error('wrong # of vnames in prt_varg');
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

case {'becm_g','recm_g'} % <====== construct variable names for these models

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

case {'bvar_g','rvar_g'} % <=================== bvar/rvar -model

fprintf(fid,'\n ***** Bayesian Vector Autoregressive Model ***** \n');
fprintf(fid,' ***** Gibbs sampling estimates             ***** \n');
if strcmp(result(1).meth,'bvar_g')
fprintf(fid,' *****    Minnesota type Prior              ***** \n');
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
fprintf(fid,' ***** Random-Walk Averaging Prior          ***** \n');
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
if strcmp(result(1).meth,'bvar_g')
y = result(j).y;
nobsy = length(y);
bhat = mean(result(j).bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(result(j).bdraw);
bstd = bstd';
tstat = bhat./bstd;
nobs = result(1).nobs;
sige = mean(result(j).sdraw)/nobs;
nvar = result(1).nvar;
nlag = result(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = result(k).y;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if result(1).nx == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat result(1).x ones(nadj,1)];
end;

yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(y,nlag,0) - yhat;
sigu = resid'*resid;
ym = y - ones(nobsy,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
ndof = length(yhat);
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(ndof-nvar);
rsqr2 = rsqr2/(ndof-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared
elseif strcmp(result(1).meth,'rvar_g')
% we compute y-hat differently here
dy = result(j).dy;
nobsy = length(dy);
bhat = mean(result(j).bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(result(j).bdraw);
bstd = bstd';
tstat = bhat./bstd;
nobs = result(1).nadj;
sige = mean(result(j).sdraw)/nobs;
nvar = result(1).nvar;
nlag = result(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = result(k).dy;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if result(1).nx == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat result(1).x ones(nadj,1)];
end;

yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(dy,nlag,0) - yhat;
sigu = resid'*resid;
ym = dy - ones(nobsy,1)*mean(dy);
ndof = length(yhat);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(ndof-nvar);
rsqr2 = rsqr2/(ndof-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared
end; % end of if-else bvar_g, rvar_g cases

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3.0f \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',rbar);
fprintf(fid,'sige          = %9.4f \n',sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,nvar);
fprintf(fid,'ndraws,nomit  = %6d,%6d \n',result(1).ndraw,result(1).nomit);
fprintf(fid,'time in secs  = %9.4f\n',result(j).time);
rmean = mean(result(j).rdraw);
if rmean ~= 0
fprintf(fid,'rmean         = %9.4f \n',rmean);
else
fprintf(fid,'r-value       = %6d  \n',result(1).r);
end;
fprintf(fid,'*******************************************************************\n');

tprob = tdis_prb(tstat,nobs);

tmp = [bhat tstat tprob];

% print out results
rnames = vstring;
for i=1:nvar
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

% ==================== end of case bvar_g


case {'becm_g','recm_g'} % <=================== becm, recm-model

fprintf(fid,'\n ***** Bayesian Error Correction Model ***** \n');
fprintf(fid,' ***** Gibbs sampling estimates        ***** \n');

if strcmp(result(1).meth,'becm_g')
fprintf(fid,' *****    Minnesota type Prior         ***** \n');
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

dy = result(j).dy;
nobsy = length(dy);
bhat = mean(result(j).bdraw);  % calculate means and std deviations
bhat = bhat';
bstd = std(result(j).bdraw);
bstd = bstd';
tstat = bhat./bstd;
nobs = result(1).nobs;
sige = mean(result(j).sdraw);
nvar = result(1).nvar;
nlag = result(1).nlag;
ymat = [];
for k=1:neqs;
ymat(:,k) = result(k).dy;
end;
xmat = mlag(ymat,nlag);
[nadj junk] = size(xmat);
if result(1).coint == 0
xmat = [xmat ones(nadj,1)];
else
xmat = [xmat result(1).x ones(nadj,1)];
end;


yhat = trimr(xmat,nlag,0)*bhat;
resid = trimr(dy,nlag,0) - yhat;
sigu = resid'*resid;
ym = dy - ones(nobsy,1)*mean(dy);
rsqr1 = sigu;
rsqr2 = ym'*ym;
ndof = length(yhat);
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(ndof-nvar);
rsqr2 = rsqr2/(ndof-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared


if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(j,:));
else
fprintf(fid,'\n Equation %3.0f \n',j);
end;

fprintf(fid,'R-squared     = %9.4f \n',rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',rbar);
fprintf(fid,'sige          = %9.4f \n',sige);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,k);
fprintf(fid,'ndraws,nomit  = %6d,%6d \n',result(1).ndraw,result(1).nomit);
fprintf(fid,'time in secs  = %9.4f\n',result(j).time);
rmean = mean(result(j).rdraw);
if rmean ~= 0
fprintf(fid,'rmean         = %9.4f \n',rmean);
else
fprintf(fid,'r-value       = %6d  \n',result(1).r);
end;
fprintf(fid,'*******************************************************************\n');

tprob = tdis_prb(tstat,nobs);

tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvar
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
error('results structure unknown to prt_varg');
end;

  
  
