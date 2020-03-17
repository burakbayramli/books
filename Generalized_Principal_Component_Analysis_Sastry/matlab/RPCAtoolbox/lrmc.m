function A = lrmc(X, tau, W)
% LRMC performs low rank matrix completion by convex optimization
% 
%     A = lrmc(X, tau, W)
% 
% performs low rank matrix completion by convex optimization:
%   min ||A||_* s.t. P_W(X) = P_W(A)
% Parameters
% X: D by N data matrix.
% tau: Parameter of the augmented Lagrangian.
% W: D by N binary matrix denoting known (1) or missing (0) entries
% Returned values
% A: Low-rank completion of the matrix X.
% Description
% Finds the low-rank approximation of a matrix X with incomplete entries as specified in W using the Low
% Rank Matrix Completion Algorithm based on the Augmented Lagrangian Method.

thr = 1e-10;
maxIter = 500;
iter = 1;
% Initialization
Z = zeros(size(X));
% Iterations
Z0 = Z;
while(1)
    A = svs(X .* W + (1/tau) * Z, 1/tau);
    Z = Z + tau * (X - A) .* W;
    % Check convergence
    error1 = max(max(abs(Z0 - Z)));
    error2 = max(max(abs((A - X) .* W)));
    error = max(error1, error2);
%     fprintf('Iteration %d, error is %f\n', iter, error);  
    if error < thr || iter > maxIter
        break;
    end
    iter = iter + 1;
    Z0 = Z;
end
fprintf('Iteration %d, error is %f\n', iter, error);
end

function A = svs(X, thr)
% Singula Value Shrinkage
    [U, S, V] = svd(X, 0);
    S = S - thr;
    S(S<0) = 0;
    A = U * S * V';
end
