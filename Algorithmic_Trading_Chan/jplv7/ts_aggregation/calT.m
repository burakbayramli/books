function T = calT(rex,s,n);
% PURPOSE: Phi-weights of ARIMA model in matrix form
% ------------------------------------------------------------
% SYNTAX: T = calT(rex,s,n);
% ------------------------------------------------------------
% OUTPUT: T :nxn lower triangular matrix, T(i,i)=1 all i
%         phi-weights are columnwise allocated (e.g., first
%         column is [1 phi(1) phi(2) ... phi(n-1)]
% ------------------------------------------------------------
% INPUT: rex: structure that contains the AR and MA operators
%        of ARIMA model, both regular and seasonal, as well as
%        the degrees of differencing
%        s: number of high frequency data points 
%        for each low frequency data points
%        n: number of data points to compute
% ------------------------------------------------------------

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

% ------------------------------------------------------------
% Receiving input

d  = rex.d;
bd = rex.bd;
ar_reg = rex.ar_reg;
ar_sea = rex.ar_sea;
ma_reg = rex.ma_reg;
ma_sea = rex.ma_sea;

% ------------------------------------------------------------
% Checks
if (d > 3) | (bd > 2)
   error ('*** EXCESSIVE NUMBER OF UNIT ROOTS ***');
end

% ------------------------------------------------------------
% Regular differences

switch d
case 0
   d_reg = 1;
case 1
   d_reg = [1 -1];
otherwise
   d_reg = conv([1 -1],[1 -1]);
   u = 2;
   while (u < d)
      d_reg = conv(d_reg,[1 -1]);
      u = u +1;
   end
end

% ------------------------------------------------------------
% Seasonal differences

aux = zeros(1,s+1); aux(1,1) = 1; aux(1,s+1) = -1;

switch bd
case 0
   d_sea = 1;
case 1
   d_sea = aux;
case 2
   d_sea = conv(aux,aux);
end
clear aux;

% ------------------------------------------------------------
% AR operators

ard = conv(d_reg,d_sea);
ar1 = conv(ar_reg,ar_sea);
ar  = conv(ar1,ard);

% ------------------------------------------------------------
% MA operators

ma  = conv(ma_reg,ma_sea);

% ------------------------------------------------------------
% Computing phi-weights of ARMA model

[phi,t] = impz(ma,ar,n); 

phi=phi(2:end); % Skipping first element (normalized to 1)

% ------------------------------------------------------------
% Generating T: nxn lower triangular matrix. Diagonal elements =1
% Phi-weights are columnwise allocated.

T = eye(n);
u = 1;
while (u < n)
   aux = phi(u) * ones(n-u,1);
   T = T + diag(aux,-u);
   u = u + 1;
end
