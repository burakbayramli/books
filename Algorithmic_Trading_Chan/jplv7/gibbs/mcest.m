function [alpha, beta] = mcest(d,n)
% PURPOSE: function called by raftery.m
% ------------------------------------------------
% SEE ALSO: coda(), prt()
% ------------------------------------------------
% REFERENCES: Geweke (1992), `Evaluating the accuracy of sampling-based
% approaches to the calculation of posterior moments', in J.O. Berger,
% J.M. Bernardo, A.P. Dawid, and A.F.M. Smith (eds.) Proceedings of
% the Fourth Valencia International Meeting on Bayesian Statistics,
% pp. 169-194, Oxford University Press
% Also: `Using simulation methods for Bayesian econometric models: 
% Inference, development and communication', at: www.econ.umn.edu/~bacc
% -----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
 
% NOTE: this code draws heavily on MATLAB programs written by
% Siddartha Chib available at: www.econ.umn.edu/~bacc
% I have repackaged it to make it easier to use.

 t = zeros(2,2);
 for i1 = 2:n;
 t(d(i1-1)+1,d(i1)+1) = t(d(i1-1)+1,d(i1)+1)+1;
 end;
 alpha = t(1,2)/(t(1,1)+t(1,2)); beta = t(2,1)/(t(2,1)+t(2,2));
