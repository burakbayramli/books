%gs_bottom_to_top   test bottom-to-top Gauss-Seidel iteration
%   IFISS scriptfile: HCE; 28 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

% Bottom-to-top line Gauss-Seidel iterative solution of system Asupg x = fsupg
% starting with zero initial guess, for problem defined on n x n grid.

% For Figures 4.2, 4.4 of Chapter 4:
%    generate benchmark problem with cd_testproblem (using prescribed
%    outflow for Example 3.1.1), plot residual using command
%       semilogy(stats(:,1),stats(:,2)/stats(1,2));
 
Q = tril(Asupg,1);
xgs = zeros(length(fsupg),1);

nf = norm(fsupg);
r = fsupg - Asupg*xgs;
nr = norm(r);
its = 0;
stats = [its,nr];
fprintf('\n%5i %15.3e\n', its, nr);

tol = 1.d-6;

[L,U] = lu(Q);
while nr/nf > tol,
   xgs = xgs + U\(L\r);   
   r = fsupg - Asupg*xgs;
   nr = norm(r);
   its = its + 1;
   stats = [stats;[its,nr]];
   fprintf('%5i %15.3e\n', its, nr);
end