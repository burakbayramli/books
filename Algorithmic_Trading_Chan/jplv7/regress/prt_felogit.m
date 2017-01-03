function prt_felogit(results,vnames,fid)
% PURPOSE: Prints output from felogit function
%----------------------------------------------------------------%
% USAGE: prt_felogit(results,vnames,fid)
% Where: results = a structure returned by a regression 
%        vnames  = optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%----------------------------------------------------------------%
% NOTES:  e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  - use prt_felogit(results,[],fid) to print to a file with no 
%    vnames
%----------------------------------------------------------------%
% RETURNS: nothing, just prints the multinomial logit results
%----------------------------------------------------------------%
% SEE ALSO: felogit
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
elseif results.meth ~= 'felogit';
    error('unknown results structure');
elseif nargin == 1
    nflag = 0; fid = 1;
elseif nargin == 2
    fid = 1; nflag = 1;
elseif nargin == 3
    nflag = 0;
    [vsize junk] = size(vnames); % user may supply a blank argument for vnames
    if vsize > 0
        nflag = 1;          
    end;
else
    error('Wrong # of arguments to prt_reg');
end;

nobs = results.nobs;
nvar = results.nvar;
N = results.N;

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

% print results
fprintf(fid,'\n');
fprintf(fid,'Fixed Effects Logit Maximum Likelihood Estimates \n');

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'McFadden R-squared    = %9.4f \n',results.rsqr);
fprintf(fid,'LR-stat (b=0, c_i=c)  = %9.4f \n',results.lratio1);
fprintf(fid,'LR p-value            = %9.4f \n',1-chis_prb(results.lratio1,nvar+N-1));
fprintf(fid,'LR-stat (c_i=c)       = %9.4f \n',results.lratio2);
fprintf(fid,'LR p-value            = %9.4f \n',1-chis_prb(results.lratio2,N-1));
fprintf(fid,'Log-Likelihood        = %9.4f \n',results.lik);
fprintf(fid,'# of iterations       = %6d   \n',results.iter);
fprintf(fid,'Convergence criterion = %9.4g \n',results.cnvg);
fprintf(fid,'Nobs, Nvars           = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'N, T                  = %6d,%6d \n',N,results.T);
fprintf(fid,'*******************************************************************************\n');
% now print coefficient estimates, z-statistics and probabilities
tout = norm_prb(results.tstatb);  % find z-stat probabilities
tmp = [results.beta results.stdb results.tstatb tout]; % matrix to be printed
% column labels for printing results
bstring = 'Coeff. '; sstring = 'Std. Err.'; tstring = 'z-stat '; pstring = 'P>|z|  ';
cnames = strvcat(bstring,sstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);


