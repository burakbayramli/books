function prt_multilogit(results,vnames,cnames,fid)
% PURPOSE: Prints output from multilogit function
%----------------------------------------------------------------%
% USAGE: prt_multilogit(results,vnames,cnames,fid)
% Where: results = a structure returned by a regression 
%        vnames  = optional vector of variable names
%        cnames  = optional vector of category names for
%                  dependent variable (ncat+1 x 1), reference
%                  category first
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%----------------------------------------------------------------%
% NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. cnames = strvcat('cat0','cat1','cat2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_multilogit(results,[],cnames,fid) to print to a file 
%  with no vnames, similarly for no cnames 
%----------------------------------------------------------------%
% RETURNS: nothing, just prints the multinomial logit results
%----------------------------------------------------------------%
% SEE ALSO: multilogit
%----------------------------------------------------------------%

% written by:
% Simon D. Woodcock,
% CISER / Economics
% 201 Caldwell Hall
% Cornell University
% Ithaca, NY 14850
% sdw9@cornell.edu

% error checking in input arguments
if ~isstruct(results)
    error('prt_reg requires structure argument');
elseif results.meth ~= 'multilogit';
    error('unknown results structure');
elseif nargin == 1
    nflag = 0; cflag = 0; fid = 1;
elseif nargin == 2
    fid = 1; nflag = 1; cflag=0;
elseif nargin == 3
    fid = 1; nflag = 1; cflag = 1;
elseif nargin == 4
    nflag = 0;
    cflag = 0;
    [vsize junk] = size(vnames); % user may supply a blank argument for vnames
    [csize junk] = size(cnames); % or cnames 
    if vsize > 0
        nflag = 1;          
    end;
    if csize > 0
        cflag = 1;
    end;    
else
    error('Wrong # of arguments to prt_reg');
end;

nobs = results.nobs;
nvar = results.nvar;
ncat = results.ncat;

% make up some generic variable names
Vname = 'Variable';
for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
end;
if (nflag == 1) % the user supplied variable names
    [tst_n nsize] = size(vnames);
    if tst_n ~= nvar+1
        fprintf(fid,'Wrong # of variable names in prt_multilogit -- check vnames argument \n');
        fprintf(fid,'will use generic variable names \n');
        nflag = 0;
    else,
        Vname = 'Variable';
        for i=1:nvar
            Vname = strvcat(Vname,vnames(i+1,:));
        end;
    end; 
end; 

% make up some generic category names
Cname = '';
for i=0:ncat
    tmp = ['category ',num2str(i)];
    Cname = strvcat(Cname,tmp);
end;
if (cflag == 1) % the user supplied category names
    [tst_n nsize] = size(cnames);
    if tst_n ~= ncat+1
        fprintf(fid,'Wrong # of category names in prt_multilogit -- check cnames argument \n');
        fprintf(fid,'will use generic category names \n');
        cflag = 0;
    else,
        Cname = '';
        for i=0:ncat
            Cname = strvcat(Cname,cnames(i+1,:));
        end;
    end; 
end; 
[junk w] = size(Cname);

% print results
fprintf(fid,'\n');
fprintf(fid,'Multinomial Logit Maximum Likelihood Estimates \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'McFadden R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'LR-stat, 2*(Lu-Lr)     = %9.4f \n',results.lratio);
fprintf(fid,'LR p-value             = %9.4f \n',1-chis_prb(results.lratio,(nvar-1)*(ncat)));
fprintf(fid,'Log-Likelihood         = %9.4f \n',results.lik);
fprintf(fid,'# of iterations        = %6d   \n',results.iter);
fprintf(fid,'Convergence criterion  = %9.4g \n',results.cnvg);
fprintf(fid,'Nobs, Nvars            = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of categories        = %6d     \n',(results.ncat + 1));
fprintf(fid,'*******************************************************************************\n');
for j = 1:ncat;
    f = (j-1)*nvar + 1;
    l = j*nvar;
    fprintf(fid,'Category: ');
    fprintf(fid,'%1c',Cname(j+1,1:w));
    fprintf(fid,'   Count: %6d \n',results.count(1,j+1));
    % now print coefficient estimates, z-statistics and probabilities
    tout = norm_prb(results.tstat_mat(:,j));  % find z-stat probabilities
    sout = sqrt(diag(results.covb(f:l,f:l)));
    tmp = [results.beta_mat(:,j) sout results.tstat_mat(:,j) tout]; % matrix to be printed
    % column labels for printing results
    bstring = 'Coeff. '; sstring = 'Std. Err.'; tstring = 'z-stat '; pstring = 'P>|z|  ';
    cnames = strvcat(bstring,sstring,tstring,pstring);
    in.cnames = cnames;
    in.rnames = Vname;
    in.fmt = '%16.6f';
    in.fid = fid;
    mprint(tmp,in);
    fprintf(fid,'-------------------------------------------------------------------------------\n');
end;
fprintf(fid,'Reference Category: ');
fprintf(fid,'%1c',Cname(1,1:w));
fprintf(fid,'   Count: %6d \n',results.count(1,1));

