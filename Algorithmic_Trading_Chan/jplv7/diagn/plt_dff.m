function plt_dff(results)
% PURPOSE: plots BKW influential observation diagnostics
%          dffits, hat-matrix diagonal, studentized residuals
%---------------------------------------------------
% USAGE: plt_dff(results)
% where: results = a structure returned by dfbeta
%---------------------------------------------------               
% RETURNS:
%        nothing, simply plots dffits, hat-matrix, s-residuals
% --------------------------------------------------
% SEE ALSO: dfbeta, plt_dfb, bkw, rdiag, diagnose
%---------------------------------------------------
% REFERENCES: Belsley, Kuh, Welsch, 1980 Regression Diagnostics

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if strcmp(results.meth,'dfbeta') ~=1
error('plt_dff requires a structure from dfbeta');
end; 

nobs = results.nobs;
nvar = results.nvar;

clf;

tt=1:nobs;

   subplot(3,1,1), plot(tt,results.dffits);
     title('dffits');
     subplot(3,1,2), plot(tt,results.stud);
     title('studentized residuals');
     subplot(3,1,3), plot(tt,results.hatdi);
     title('hat-matrix diagonals');
     

