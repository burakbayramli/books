%gs_four   test four-directional Gauss-Seidel iteration
%   IFISS scriptfile: HCE; 28 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

% Gauss-Seidel four-directional iterative solution of system Asupg x = fsupg
% starting with zero initial guess, for problem defined on n x n grid.

n = sqrt(size(Asupg,1));

Q1 = tril(Asupg,1);
Q2 = diag(diag(Asupg,0)) + diag(diag(Asupg,-n),-n) + diag(diag(Asupg,n),n) ...
   + diag(diag(Asupg,-(n+1)),-(n+1)) + diag( diag(Asupg,-1),-1)        ...
   + diag(diag(Asupg,(n-1)),(n-1));
Q3 = triu(Asupg,-1);
Q4 = diag(diag(Asupg,0)) + diag(diag(Asupg,-n),-n) + diag(diag(Asupg,n),n) ...
   + diag(diag(Asupg,(n+1)),(n+1)) + diag( diag(Asupg,1),1)            ...
   + diag(diag(Asupg,-(n-1)),-(n-1));
xgs = zeros(length(fsupg),1);

nf = norm(fsupg);
r = fsupg - Asupg*xgs;
nr = norm(r);
its = 0;
stats = [its,nr];
fprintf('\n%5i %15.3e\n', its, nr); 

tol = 1.d-6;

[L1,U1] = lu(Q1);
[L2,U2] = lu(Q2);
[L3,U3] = lu(Q3);
[L4,U4] = lu(Q4);

while nr/nf > tol,
   xgs = xgs + U1\(L1\r);   r = fsupg - Asupg*xgs;
   xgs = xgs + U2\(L2\r);   r = fsupg - Asupg*xgs; 
   xgs = xgs + U3\(L3\r);   r = fsupg - Asupg*xgs;
   xgs = xgs + U4\(L4\r);   r = fsupg - Asupg*xgs;
   nr = norm(r);
   its = its + 1;
   stats = [stats;[its,nr]];
   fprintf('%5i %15.3e\n', its, nr); 
end