function B = jx(A,r,s)
% syntax: B = jx(A,r,s)
% input: matrix A, integers r,s
% perform a jordan exchange with pivot A(r,s)

[m,n] = size(A); B = zeros(m,n);
I = [1:r-1,r+1:m]; J = [1:s-1,s+1:n];

% update pivot row
B(r,s) = 1.0/A(r,s); B(r,J) = -A(r,J)/A(r,s);

% update pivot column
B(I,s) = A(I,s)/A(r,s);

% update remainder of tableau
B(I,J) = A(I,J)-B(I,s)*A(r,J);
return;
