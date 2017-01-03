function delta=aggreg_test(Y,y,ta,s,lim);
% PURPOSE: Test of temporal aggregation
% -----------------------------------------------------------------------
% SYNTAX: delta=aggreg_tes(Y,y,ta,s,lim);
% -----------------------------------------------------------------------
% OUTPUT: Low frequency discrepancies (as percentage)
% -----------------------------------------------------------------------
% INPUT: Y: Nx1 ---> vector of low frequency data
%        y: nx1 ---> vector of high frequency data temporally disaggregated
%        ta: type of disaggregation
%            ta=1 ---> sum (flow)
%            ta=2 ---> average (index)
%            ta=3 ---> last element (stock) ---> interpolation
%        s: number of high frequency data points for each low frequency data points 
%            s= 4 ---> annual to quarterly
%            s=12 ---> annual to monthly
%            s= 3 ---> quarterly to monthly
%        lim: threshold to trigger warning (in percentage)
% -----------------------------------------------------------------------
% LIBRARY: aggreg
% -----------------------------------------------------------------------

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

[N,M] = size(Y);

% -----------------------------------------------------------------------
% Generation of aggregation matrix C

C = aggreg(ta,N,s);

Yy = C*y;

delta = 100*((Y-Yy)./Y);

if (max(abs(delta)) > lim)
   disp (' *** NON ADMISSIBLE DISCREPANCY *** ');
end
