function [M,L] = gauss(A)
%
%   [M,L] = gauss(A)
%    
%      Performs regular Gaussian elimination on matrix A.
%      Halts if diagonal entry is 0
%      M -- row echelon form
%      L -- special lower triangular matrix
%      A = L * M
%


[m,n] = size(A);
L = eye(m)
M = A
for i = 1:m-1
   if M(i,i) == 0 error('Pivot is 0'); return; end
   for k = i+1:m
       L(k,i) = M(k,i)/M(i,i)
	   for j = i+1:n
	       M(k,j) = M(k,j) - L(k,i)* M(i,j);
	   end
	   for j = 1:i
	       M(k,j) = 0;
	   end
	   M
    end
end
