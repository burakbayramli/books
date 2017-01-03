function prt_panel(result,vnames,fid)
% PURPOSE: Prints Panel models output
%----------------------------------------------------------------------------------------
% USAGE:  prt_panel(result,vnames,fid)        
%    where: 
%    		 results = a structure returned by:
%                      ppooled,pfixed,prandom
%         vnames  = optional vector of variable names
%             fid = (optional) file-id for printing to a file
%                   (defaults to the MATLAB command window)
%----------------------------------------------------------------------------------------               
% NOTE: - constant term is added automatically to vnames list
%       - you may use prt_panel(results,[],fid) to print
%         output to a file with no vnames                   
%----------------------------------------------------------------------------------------               

if nargin < 1; error('wrong # of arguments to prt_panel'); end;
if nargin > 3; error('wrong # of arguments to prt_panel'); end;

if ~isstruct(result);
 error('prt_panel requires a panel model results structure');
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

% find nobs, nvar (used throughout all types of models)
nobs = result.nobs;
nvars = result.nvar;
cconst = result.crconst;


% set up variable names used in all panel models
% --------------------------------------------------------
% # no variable names supplied
% --------------------------------------------------------
if nflag == 0
   Vname = [];

   for i=1:nvars;
   if i < nvars
   Vname{i} = str2mat(['variable ',num2str(i)]);
   else
   Vname{i} = 'constant ';
	end;
	end;

end; % end of if nflag == 0

% --------------------------------------------------------
% user supplies variable names
% --------------------------------------------------------
if (nflag == 1)
Vname = [];

[namesize namewidth] = size(vnames);
if namesize ~= nvars + cconst 
   error('wrong # of vnames in prt_panel');
end;

   for i=1:nvars;
   if i < nvars
   Vname{i} = vnames(i+1,:);
   else
   Vname{i} = 'constant ';
   end;
   end;

end; % end of if nflag == 1


% column headers used throughout
vstring = 'Variable';
bstring = 'Coefficient';
tstring = 't-statistic';
pstring = 't-probability';
istring = 'Individual';
cpstring = 'Components';

switch result.meth

case {'ppooled'} % <=================== Pooled-model

fprintf(fid,'\n ***** Pooled Model ***** \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
else
fprintf(fid,'\n Dependent Variable %3d \n');
end;

fprintf(fid,'R-squared     = %9.4f \n',result.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result.rbar);
fprintf(fid,'sige          = %9.4f \n',result.sige);
fprintf(fid,'Time	      = %9.4f \n',result.time);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,nvars);
fprintf(fid,'******************************************************************\n');

% pull out results
bhat  = result.beta;
tstat = result.tstat;
tprob = result.tprob;
tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvars
   tmpn{i} = [Vname{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

% ==================== end of case pooled

case {'pfixed'} % <=================== Fixed Effects-model

fprintf(fid,'\n ***** Fixed Effects Model ***** \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
else
fprintf(fid,'\n Dependent Variable %3d \n');
end;

fprintf(fid,'R-squared     = %9.4f \n',result.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result.rbar);
fprintf(fid,'sige          = %9.4f \n',result.sige);
fprintf(fid,'Time	      = %9.4f \n',result.time);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,nvars);
fprintf(fid,'******************************************************************\n');

% pull out  results
bhat  = result.beta;
tstat = result.tstat;
tprob = result.tprob;
tmp = [bhat tstat tprob];

%Adjust variable names;

if nflag == 0  % # no variable names supplied
   Vname = [];
   for i=1:nvars;
   Vname{i} = str2mat(['variable ',num2str(i)]);
   end;
elseif (nflag == 1) % user supplies variable names
   Vname = [];
   [namesize namewidth] = size(vnames);
if namesize ~= nvars + cconst 
   error('wrong # of vnames in prt_panel');
end;

   for i=1:nvars;
   Vname{i} = vnames(i+1,:);
   end;

end; % end of if nflag 

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvars
   tmpn{i} = [Vname{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

% individual intercepts

fprintf(fid,'******************************************************************\n');

% pull out  results
indcomp = result.iintc;
idin    =  result.idy;
[ind junk] = size(idin);
tmpi = [idin indcomp];

% print out results
ini.cnames = strvcat(istring, cpstring);
ini.fmt = '%16.6f';
ini.fid = fid;
mprint(tmpi,ini); 

% ==================== end of case fixed

case {'prandom'} % <=================== Random Effects-model

fprintf(fid,'\n ***** Random Effects Model ***** \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
else
fprintf(fid,'\n Dependent Variable %3d \n');
end;

fprintf(fid,'R-squared     = %9.4f \n',result.rsqr);
fprintf(fid,'Rbar-squared  = %9.4f \n',result.rbar);
fprintf(fid,'sige          = %9.4f \n',result.sige);
fprintf(fid,'sigu          = %9.4f \n',result.sigu2);
fprintf(fid,'Time	      = %9.4f \n',result.time);
fprintf(fid,'Nobs, Nvars   = %6d,%6d \n',nobs,nvars);
fprintf(fid,'******************************************************************\n');

% pull out  results
bhat  = result.beta;
tstat = result.tstat;
tprob = result.tprob;
tmp = [bhat tstat tprob];

% print out results
in.cnames = strvcat(bstring,tstring,pstring);
rnames = vstring;
for i=1:nvars
   tmpn{i} = [Vname{i}];
rnames = strvcat(rnames,tmpn{i});
end;
in.rnames = rnames;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in); 

% individual intercepts

fprintf(fid,'******************************************************************\n');

% pull out  results
indcomp = result.iintc;
idin    =  result.idy;
[ind junk] = size(idin);
tmpi = [idin indcomp];

% print out results
ini.cnames = strvcat(istring, cpstring);
ini.fmt = '%16.6f';
ini.fid = fid;
mprint(tmpi,ini); 

% ==================== end of case Random

otherwise
error('results structure unknown to prt_panel');
end;

  
  
