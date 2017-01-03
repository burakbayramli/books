function A = trid(a,b,c,n)
%
%   A = trid(a,b,c,n)
%    
%     A -- n x n tridiagonal matrix with  a,b,c along diagonals 
%

A = a*diag(ones(n-1,1),-1) + b*diag(ones(n,1)) + c*diag(ones(n-1,1),1);
