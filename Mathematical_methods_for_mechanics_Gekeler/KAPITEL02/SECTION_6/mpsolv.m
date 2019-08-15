function x = mpsolv(A,b);
% calculates solution x = mpinv(A)*b
% for rank-maximal (n,n+1)-Matrix A
[m,n] = size(A);
if n ~= m+1 disp('A nor a (n,n+1)-matrix'), return
end
R = triu(qr([A,b]));
C = R(1:m,m+2); T = R(1:m,m+1); S = R(1:m,1:m);
G = [-T,C]; U = S\G;
V = -[U(:,1);1]\[U(:,2);0];
x = [U(:,1);1]*V + [U(:,2);0];
