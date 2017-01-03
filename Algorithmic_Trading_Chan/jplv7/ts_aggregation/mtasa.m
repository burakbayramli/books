function gZ = mtasa(Z,s);
% PURPOSE: Compute the year-on-year rate of growth of a vector time series
% ------------------------------------------------------------------------
% SYNTAX:  gZ = mtasa(Z,s);
% ------------------------------------------------------------
% OUTPUT: gZ ---> yoy rate of growth. First s obs are NaN
% ------------------------------------------------------------
% INPUT:  Z : nkx vector time series to be filtered
%         s : 1x1 number of periods per year
% ------------------------------------------------------------

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% c/ Rosario Pino, 14-16. Office 15.34
% 28046 - Madrid (SPAIN)

[n,k] = size(Z);

% Preallocation
gZ = NaN * ones(n,k);

% Basic loop
for j=1:k
    gz = tasa(Z(:,j),s);
    gZ(:,j) = gz;
end
