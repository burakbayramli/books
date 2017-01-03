function [L, a, Zti] = ztransi(first, last, n, L, a, Zti)
% ZTRANSI  Updates integral Z-transform for L;
%	        only column `first' until `last'.
%	        The output is the inverse of Z transpose.

% first   first column to be updated
% last	 last column to be updated
% n	    dimension of the system
% L	    lower triangular matrix L
% a	    Z (transposed) a, with a the vector of unknowns
% Zti	    the inverse Z transposed transformation matrix

% Paul de Jonge, Delft Geodetic Computing Centre (LGR)
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

for i = last:-1:first
   for j = i+1:n
      mu = round(L(j,i));
      if mu ~= 0
         L(j:n,i) = L(j:n,i) - mu * L(j:n,j);
         Zti(1:n,j) = Zti(1:n,j) + mu * Zti(1:n,i);
         a(i) = a(i) - mu * a(j);
      end
   end
end
%%%%%%%%%% end ztransi.m  %%%%%%%%%%%%%%%%%%%%%%%
