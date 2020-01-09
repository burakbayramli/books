function [x,stats] = dikin_solver_sdp(c,A,B,x0)
% uses Dikin's method to solve SDPs of the form
%   minimize    c^T x
%   subject to  \sum_{i=1}^n A_i x_i <= B
% 
% input:
%   c: n by 1 array
%   A: m by m by n array such that A(:,:,i) is A_i. A_i is symmetric
%   B: m by m symmetric array
%   x0: starting point. should be primal feasible
%
% output:
%   x: solution
%   stats: history of residuals for each iteration
%
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014

MAXITER = 50;
TOL = 1e-4;
[~,m,n] = size(A);
stats = [];
x = x0;
for k = 1:MAXITER
    S = zeros(m,m);
    for i=1:n
        S = S + A(:,:,i)*x(i);
    end
    S = B - S;
    
    SinvA = reshape(S\reshape(A,m,m*n),m,m,n);
    SinvASinv = permute(reshape((reshape(permute(SinvA,[2,1,3]),m,m*n)'/S)',m,m,n),[2,1,3]);
      
    H = zeros(n,n);
    for i=1:n
       for j = i:n 
           H(i,j) = trace(SinvA(:,:,i)*SinvA(:,:,j));
           H(j,i) = H(i,j);
       end
    end 
    
    s = -H\c;
    
    % dual variable Z
    Z = zeros(m,m);
    for i=1:n
        Z = Z + SinvASinv(:,:,i)*s(i);
    end
    
    x = x + s/sqrt(-c'*s);
    
    stats(end+1,:) = [pos(-min(eig(Z))), abs(c'*x + trace(B*Z))];
    if all(stats(end,:) <= TOL)
        break;
    end
end

end