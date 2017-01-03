function r=conta(aux,f);
% PURPOSE: determine number of non-f elements in polynomial
% ------------------------------------------------------------
% SYNTAX: r=conta(aux,f);
% ------------------------------------------------------------
% OUTPUT: r: 1x1 number of non-f elements in aux
% ------------------------------------------------------------
% INPUT: aux: polynomial of unknown length
%        f: selected value for comparison
% ------------------------------------------------------------
% LIBRARY:

% written by:
% Enrique M. Quilis
% Instituto Nacional de Estadistica
% Paseo de la Castellana, 183
% 28046 - Madrid (SPAIN)

r=0;
u=length(aux);
for i=1:u
   if (aux(i) ~= f);
      r=r+1;
   end
end

      