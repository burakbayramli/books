function [c]=aggreg_v(op1,s)
% PURPOSE: Generate a temporal aggregation vector
% ------------------------------------------------------------
% SYNTAX: c=aggreg_v(op1,s);
% ------------------------------------------------------------
% OUTPUT: c: 1xs temporal aggregation vector
% ------------------------------------------------------------
% INPUT:  op1: type of temporal aggregation 
%         op1=1 ---> sum (flow)
%         op1=2 ---> average (index)
%         op1=3 ---> last element (stock) ---> interpolation
%         op1=4 ---> first element (stock) ---> interpolation
%         s: number of high frequency data points 
%            for each low frequency data points (freq. conversion)
% ------------------------------------------------------------
% LIBRARY:
% ------------------------------------------------------------
% SEE ALSO: aggreg, temporal_agg

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

% ------------------------------------------------------------
% Generation of aggregation vector c

switch op1
case 1   
   c=ones(1,s);
case 2
   c=ones(1,s)./s;
case 3
   c=zeros(1,s);
   c(s)=1;
case 4
   c=zeros(1,s);
   c(1)=1;
otherwise
   error ('*** IMPROPER TYPE OF TEMPORAL DISAGGREGATION ***');
end
