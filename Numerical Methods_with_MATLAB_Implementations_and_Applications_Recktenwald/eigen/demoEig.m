function demoEig(A)
% demoEig  Example usage of built-in eig and eigs functions

if nargin<1
  A = tridiag(5,2,-1);  %  NMM toolbox, create (-1,2,-1) tridiagonal matrix 
end

[V,L] = eigSort(A,-1);       %  eigenvalue/vector pairs in descending order

fprintf('   k       error\n');
for k=1:size(L,1)            %  verify that A*v = lambda*v
  v = A*V(:,k)/L(k,k);
  err = norm(v-V(:,k));
  fprintf('%4d  %12.2e\n',k,err)
end
