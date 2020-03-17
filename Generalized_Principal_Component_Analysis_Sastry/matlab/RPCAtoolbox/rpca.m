function [L, E] = rpca(X, method, tau, lambda)
% RPCA performs robust PCA by convex optimization.
% 
%     A = lrmc(X, tau, W)
% 
% performs robust PCA by convex optimization:
%   min ||L||_* + \lambda ||E||_1 s.t. X = L + E     or
%   min ||L||_* + \lambda ||E||_{2,1} s.t. X = L + E
% Parameters
% X: D by N data matrix.
% tau: Parameter of the augmented Lagrangian.
% method: ¡¯L1¡¯ for gross errors or ¡¯L21¡¯ for outliers
% Returned values
% L: Low-rank completion of the matrix X.
% E: Matrix of errors.
% Description
% Solves the optimization problem using ADMM
% By Chong You

[D, N] = size(X);
% Set parameters
% if strcmp(method, 'L1')
%     lambda = 1 / sqrt( max([D, N]) );
% elseif strcmp(method, 'L21')
%     lambda = 0.5; %3 / ( 7 * sqrt(gamma*N) );% gamma = fraction of outlier
% end
thr = 1e-5;
maxIter = 20;
% Initialization
E = zeros(size(X));
Y = zeros(size(X));
% Iterations
iter = 1;
while(1)
    % Update L
    L = svs(X - E + (1/tau) * Y, 1/tau);
    % Update E
    A = X - L + Y / tau;
    if strcmp(method, 'L1')
        E = max( abs(A) - lambda/tau, 0 ) .* sign(A);
    elseif strcmp(method, 'L21')
        normA = sqrt( sum(A .^2, 1) );
        E = A * diag( max( normA - lambda/tau, 0) ./ normA );
    end
    % Update Y
    Y = Y + tau * (X - L - E);
    % Check convergence
    error = max(max(abs(X - L - E)));
%     fprintf('Iteration %d, error is %f\n', iter, error);  
    if error < thr || iter > maxIter
        break;
    end
    iter = iter + 1;
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
