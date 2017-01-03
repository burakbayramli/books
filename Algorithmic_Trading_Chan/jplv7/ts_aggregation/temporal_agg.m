function [y]=temporal_agg(z,op1,s)
% PURPOSE: Temporal aggregation of a time series 
% ------------------------------------------------------------
% SYNTAX: [y]=temporal_agg(z,op1,s);
% ------------------------------------------------------------
% OUTPUT: y: Nx1 temporally aggregated series
% ------------------------------------------------------------
% INPUT:  z: nx1 ---> vector of high frequency data
%         op1: type of temporal aggregation 
%         op1=1 ---> sum (flow)
%         op1=2 ---> average (index)
%         op1=3 ---> last element (stock) ---> interpolation
%         op1=4 ---> first element (stock) ---> interpolation
%         s: number of high frequency data points 
%            for each low frequency data points
% ------------------------------------------------------------
% LIBRARY: aggreg
% ------------------------------------------------------------

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

[n,m] = size(z);

% ------------------------------------------------------------
% Computes the number of low frequency points. 
% Low frequency periods should be complete

N = n/s;

C=aggreg(op1,N,s);
y=C*z;

