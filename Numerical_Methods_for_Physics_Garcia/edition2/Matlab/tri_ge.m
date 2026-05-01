function x = tri_ge(a,b)
% Function to solve b = a*x by Gaussian elimination where
% the matrix a is a packed tridiagonal matrix
% Inputs
%   a    Packed tridiagonal matrix, N by N unpacked
%   b    Column vector of length N
% Output 
%   x    Solution of b = a*x; Column vector of length N

%* Check that dimensions of a and b are compatible
[N,M] = size(a);
[NN,MM] = size(b);
if( N ~= NN | MM ~= 1)
  error('Problem in tri_GE, inputs are incompatible');
end

%* Unpack diagonals of triangular matrix into vectors
alpha(1:N-1) = a(2:N,1);
beta(1:N) = a(1:N,2);
gamma(1:N-1) = a(1:N-1,3);

%* Perform forward elimination
for i=2:N
  coeff = alpha(i-1)/beta(i-1);
  beta(i) = beta(i) - coeff*gamma(i-1);
  b(i) = b(i) - coeff*b(i-1);
end

%* Perform back substitution
x(N) = b(N)/beta(N);
for i=N-1:-1:1
  x(i) = (b(i) - gamma(i)*x(i+1))/beta(i);
end
x = x.';   % Transpose x to a column vector
return;
