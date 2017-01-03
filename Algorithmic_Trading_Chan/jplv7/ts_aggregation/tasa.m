function gz = tasa(z,s);
% PURPOSE: Compute the year-on-year rate of growth
% ------------------------------------------------------------
% SYNTAX:  gz=tasa(z,s);
% ------------------------------------------------------------
% OUTPUT: gz ---> yoy rate of growth. First s obs are NaN
% ------------------------------------------------------------
% INPUT:  z : time series to be filtered
%         s : number of periods per year
% ------------------------------------------------------------

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% c/ Rosario Pino, 14-16. Office 15.34
% 28046 - Madrid (SPAIN)

% ------------------------------------------------------------
% Generation of differenced series: zd(t)=z(t)-z(t-s)

a = zeros(1,s+1);
a(1) = 1; a(s+1)=-1;    % Generation of MA filter as (1 - B**s)
b = [1];                              % Generation of AR filter as 1

dz = filter(a,b,z);
dz(1:s) = NaN;      % The first s obs. are converted in Not a Number

% ------------------------------------------------------------
% Generation of lagged series: zs(t)=z(t-s)

a = zeros(1,s+1);
a(s+1) = 1;    % Generation of MA filter as B**s
b = [1];       % Generation of AR filter as 1

z_s = filter(a,b,z);
z_s(1:s) = NaN;      % The first s obs. are converted in Not a Number (NaN)

gz = 100 * (dz ./ z_s);
