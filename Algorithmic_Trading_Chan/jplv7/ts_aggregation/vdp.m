function z = vdp(y,sigma,A);
% PURPOSE: Balancing by means of van der Ploeg method
% ------------------------------------------------------------
% SYNTAX: z = vdp(y,sigma,A);
% ------------------------------------------------------------
% OUTPUT: z : kx1 vector of balanced variables
% ------------------------------------------------------------
% INPUT:  y     : kx1 vector of unbalanced variables (initial estimates)
%         sigma : kxk VCV of initial estimates
%         A     : kxm matrix of linear constraints
% ------------------------------------------------------------
% REFERENCE: Van der Ploeg, F.(1982)"Reliability and the adjustment 
% of sequences of large economic accounting matrices",Journal of 
% the Royal Statistical Society, series A, vol. 145, n. 2, p. 169-194.

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)


disc = A' * y;    % Discrepancy

AUX = inv(A'*sigma*A);

z = y - sigma*A*AUX*disc;  % LS balanced estimation




