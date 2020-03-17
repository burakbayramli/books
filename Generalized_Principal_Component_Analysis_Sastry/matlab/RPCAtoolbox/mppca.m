function [mu, Ud, sigma] = mppca(X, d, W)
% Function [mu,Ud,sigma]=mppca(X,d,W)
% Parameters
% X D by N data matrix.
% d Number of principal components.
% W D by N binary matrix denoting known (1) or missing (0) entries
% Returned values
% mu Mean of the data.
% Ud Basis for the subspace (does not need to be orthonormal).
% sigma Standard deviation of the noise.
% Description
% Finds the parameters of the PPCA model from the data X with incomplete entries
% as speci?ed in W using the Expectation Maximization algorithm.

[D, N] = size(X);
thr = 1e-10;
maxIter = 100;
iter = 1;
% Initilization
X = X .* W;
mu = mean(X, 2);
Sigma = ( bsxfun(@minus, X, mu) * bsxfun(@minus, X, mu)' ) / N;
% Iterations
mu0 = mu;
S0 = Sigma;
while(1)
    % Compute X
    for iN = 1:N
        w = logical( W(:, iN) ); % mask for this column.
        if any(~w)
            X(~w, iN) = Sigma(~w, w) / Sigma(w, w) * (X(w, iN) - mu(w));
        end
    end
    % Compute mu
    mu = mean(X, 2);
    % Compute S
    S = ( bsxfun(@minus, X, mu) * bsxfun(@minus, X, mu)' ) / N;
    for iN = 1:N
        w = logical( W(:, iN) ); % mask for this column.
        if any(~w)
            S(~w, ~w) = S(~w, ~w) + ( Sigma(~w, ~w) - Sigma(~w, w) / Sigma(w, w) * Sigma(w, ~w) )/N;
        end
    end
    % Compute Ud, sigma
    [U, Lambda] = eigs(S, d);
    sigma = sqrt( (trace(S) - sum(diag(Lambda))) / (D - d) );
    Ud = U * diag( sqrt(diag(Lambda) - sigma ^2) );
    % Compute Sigma
    Sigma = Ud * Ud' + sigma^2 * eye(D);
    
    % Check convergence
    error = max( [norm( mu - mu0, Inf), max(max(abs( S - S0 )))] );
%     fprintf('Iteration %d, error is %f\n', iter, error);
    if error < thr || iter > maxIter
        break;
    end
    mu0 = mu; 
    S0 = S;
    iter = iter + 1;
end
fprintf('Iteration %d, error is %f\n', iter, error);
end
