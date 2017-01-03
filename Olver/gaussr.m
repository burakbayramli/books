function [M,L] = gaussr(A)
%
%   [M,L] = gaussr(A)
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
	   M(k,:) = M(k,:) - L(k,i)* M(i,:)
   end
end
