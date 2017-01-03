function [D]=dif(d,n);
% PURPOSE: Generate the difference operator in matrix form
% ------------------------------------------------------------
% SYNTAX: D=dif(d,n);
% ------------------------------------------------------------
% OUTPUT: D: nxn difference operator in matrix form
% ------------------------------------------------------------
% INPUT:  d : degree of differencing
%         n : dimension of filter matrix
% ------------------------------------------------------------
% NOTE: d initial conditions are assumed to be zero

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

switch d
    case 0 % Levels
        D = eye(n);
    case 1 % First differences
        % Initial conditions: y(0)=0
        D = eye(n) + diag(-ones((n-1),1),-1);
    case 2 % Second differences
        % Initial conditions: y(0)=y(-1)=0
        D = eye(n) + diag(-2*ones((n-1),1),-1) + diag(ones((n-2),1),-2);
    otherwise 
        error (' *** IMPROPER DEGREE OF DIFFERENCING *** ');
end
