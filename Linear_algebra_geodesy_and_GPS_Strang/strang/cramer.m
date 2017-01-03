function x = cramer(A,b)
%CRAMER	Solve linear system by Cramer's Rule.
%	x = CRAMER(A,b) solves the square system A*x = b.

if det(A) == 0
   error('Matrix is singular')
end
[n,n] = size(A);
for j = 1:n
   B = A;
   B(:,j) = b;
   x(j) = det(B)/det(A);
end
x = x';
