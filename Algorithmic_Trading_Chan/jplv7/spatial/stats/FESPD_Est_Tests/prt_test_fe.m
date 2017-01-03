function prt_test_fe(results,fid)
% PURPOSE : Print output of the tests for spatial autocorrelation in a
% fixed effects panel data model as well as for estimation procedure for
% spatial fixed effects model with the Lee and Yu methodology (Journal of
% Econometrics, 2009, 154, 165-185)
% USAGE: prt_fe(results,vnames,fid)
% Where: results = a structure returned by a spatial regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_fe(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the spatial regression results
% --------------------------------------------------

% written by N. Debarsy* and C. Ertur**
% * University of Namur
%   Centre de recherches en Economie Régionale et Politique Economique (CERPE)
%   Rempart de la vierge, 8
%   5000 Namur, Belgium
%   nicolas.debarsy@fundp.ac.be

%** Université d'Orléans
%   UFR Droit-Economie-Gestion
%   Laboratoire d'Economie d'Orléans - UMR 6221 CNRS
%   Domaine Universitaire
%   Rue de Blois - BP 6739
%   45067 ORLEANS Cedex 2, France
%   cem.ertur@univ-orleans.fr

%  This function is partly based on the James P. LeSage's function
%  prt_spat.m 

if ~isstruct(results)
 error('prt_sar requires structure argument');
elseif nargin == 1
 nflag = 0; fid = 1;
elseif nargin == 2
 fid = 1; nflag = 1;
elseif nargin == 3
 nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
else
 error('Wrong # of arguments to prt_test_fe');
end;


switch results.meth
case{'lm_f_err'}
fprintf(fid,'LM test for spatial correlation in residuals of a fixed effects panel data model \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;    
case{'lm_f_sar'}
fprintf(fid,'LM test for a SAR specification in a fixed effects panel data model\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability', 'Chi2_1 1% value');
%in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;    
case{'lm_f_joint'}
fprintf(fid,'Joint LM test in a fixed effects panel data model\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability', 'Chi2_2 1% value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;    

case{'lm_f_sar_c'}
fprintf(fid,'LM test for spatially autocorrelated errors when a SAR is already accounted for \n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;    

case{'lm_f_err_c'}
fprintf(fid,'LM test for a SAR when spatially autocorrelated errors are already accounted for\n');
variable = ' ';
in.rnames = strvcat(variable,'LM value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lm
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;    

case{'lr_f_err'}
fprintf(fid,'LR test for spatial correlation in residuals of a fixed effects panel data model\n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability','Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lr
       results.prob
       results.chi_1];
mprint(full(mat),in);
case{'lr_f_sar'}
fprintf(fid,'LR test for a SAR specification in a fixed effects panel data model \n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lr
       results.prob
       results.chi_1];
mprint(full(mat),in);

case{'lr_f_joint'}
fprintf(fid,'Joint LR test in a fixed effects panel data model\n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability','Chi2_2 1% value');
in.fmt = '%16.8f';
mat = [results.lr
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;  

case{'lr_f_sar_c'}
fprintf(fid,'LR test for spatially autocorrelated errors when a SAR is already accounted for\n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lr
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;  

case{'lr_f_err_c'}
fprintf(fid,'LR test for a SAR when spatially autocorrelated errors are already accounted for\n');
variable = ' ';
in.rnames = strvcat(variable,'LR value','Marginal Probability', 'Chi2_1 1% value');
in.fmt = '%16.8f';
mat = [results.lr
       results.prob
       results.chi_1];
mprint(full(mat),in);
return;  

end;
