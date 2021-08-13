%HELPME_STOPPING optimal iterative solvers interactive help 
%   IFISS scriptfile: DJS; 20 January 2011. 
% Copyright (c) 2010 D.J. Silvester, V. Simoncini, Q. Liao
fprintf(' \n');
fprintf('  To solve the current discrete system, and to check the algebraic error bounds\n');
fprintf('  call the function file: minres_esterror: see help minres_esterror ...\n\n')
lookfor minres
fprintf(' \n\n');
fprintf('  If Q2-P1 approximation is used then you can test the optimal\n');
fprintf('  solver (EST_MINRES) by calling the script file:  itsolve_stokes\n\n');
help itsolve_stokes
fprintf(' \n');
