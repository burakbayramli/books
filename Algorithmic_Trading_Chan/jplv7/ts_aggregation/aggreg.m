function [C]=aggreg(op1,N,s)
% PURPOSE: Generate a temporal aggregation matrix
% ------------------------------------------------------------
% SYNTAX: C=aggreg(op1,N,s);
% ------------------------------------------------------------
% OUTPUT: C: NxsN temporal aggregation matrix
% ------------------------------------------------------------
% INPUT:  op1: type of temporal aggregation 
%         op1=1 ---> sum (flow)
%         op1=2 ---> average (index)
%         op1=3 ---> last element (stock) ---> interpolation
%         op1=4 ---> first element (stock) ---> interpolation
%         N: number of low frequency data
%         s: number of high frequency data points 
%            for each low frequency data points (freq. conversion)
% ------------------------------------------------------------
% LIBRARY:
% ------------------------------------------------------------
% SEE ALSO: temporal_agg

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

% ------------------------------------------------------------
% Generation of aggregation matrix C=I(N) <kron> c

c = aggreg_v(op1,s);

% ------------------------------------------------------------
% Temporal aggregation matrix

C=kron(eye(N),c);

