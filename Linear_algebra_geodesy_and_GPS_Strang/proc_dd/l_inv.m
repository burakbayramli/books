function Lm = l_inv(n, L)
% L_INV    computes the inverse of a lower triangular matrix

% n      dimension of the matrix
% L      lower triangular matrix
% Lm     inverse of L; Lm is also lower triangular

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded for MATLAB by Kai Borre 12-04-96

vec = zeros(n,1);
for i = 1:n
   vec(1:i-1,1) = L(i,1:i-1)';
   for j = 1:i-1
      Lm(i,j) = - Lm(j:i-1,j)'*vec(j:i-1,1)/L(i,i);
   end
   Lm(i,i) = 1/L(i,i);
end
%%%%%%%%%%%% end l_inv.m %%%%%%%%%%%%%%%%%%%%%%%%%
