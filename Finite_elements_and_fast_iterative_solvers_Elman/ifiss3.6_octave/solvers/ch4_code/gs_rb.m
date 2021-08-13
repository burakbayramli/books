%gs_rb   test line red-black Gauss-Seidel iteration
%   IFISS scriptfile: HCE; 28 January 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

% Line red-black Gauss-Seidel iterative solution of system Asupg x = fsupg
% starting with zero initial guess for problem defined on n x n grid.  
% Lines oriented in horizontal direction.

% For Figure 4.4 of Chapter 4:
%    generate benchmark problem with cd_testproblem (using prescribed
%    outflow for Example 3.1.1), plot residual using command
%       semilogy(stats(:,1),stats(:,2)/stats(1,2));

perm = [];

n = sqrt(size(Asupg,1));
if mod(n,2)==1, maxred = n;   maxblk = n-1;
else            maxred = n-1; maxblk = n;
end

for i=1:2:maxred,
   perm = [perm; [(i-1)*n+1:i*n]'];
end
for i=2:2:maxblk,
   perm = [perm; [(i-1)*n+1:i*n]'];
end

Arb = Asupg(perm,perm);
frb = fsupg(perm);

Q = tril(Arb,1);
xgs = zeros(length(frb),1);

nf = norm(frb);
r = frb - Arb*xgs;
nr = norm(r);
its = 0;
stats = [its,nr];
fprintf('\n%5i %15.3e\n', its, nr);

tol = 1.d-6;

[L,U]=lu(Q);
while nr/nf > tol,
   xgs = xgs + U\(L\r);
   r = frb - Arb*xgs;
   nr = norm(r);
   its = its + 1;
   stats = [stats;[its,nr]];
   fprintf('%5i %15.3e\n', its, nr); %    [its,nr]
end

xnat=zeros(length(xgs),1); xnat(perm) = xgs;