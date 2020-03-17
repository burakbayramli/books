function [mu, Ud, Y] = rpca_rls(X, d, epsilon0)
% Function [mu,Ud,Y]=rpca rls(X,d)
% Parameters
% X D by N data matrix.
% d Number of principal components.
% Returned values
% mu Mean of the data.
% Ud Basis for the subspace.
% Description
% Finds the parameters of the PCA model  and Ud and the low-dimensional representation using re-weighted
% least squares

% Set parameters
[D, N] = size(X);
thr = 1e-10;
maxIter = 10;
% Initialization
[coeff,score,~,~,~,mu] = pca(X', 'NumComponents', d);
mu = mu';
Ud = coeff;
Y = score';
% Iterations
X0 = bsxfun(@plus, mu, Ud*Y);
iter = 1;
while(1)
    % Update weight
    epsilon = bsxfun(@minus, X, mu) - Ud * Y;
    W = epsilon0 ^2 ./ (epsilon0 ^2 + epsilon .^2);
    % Update mu
    mu = sum( W .* (X - Ud * Y),  2) ./ sum(W, 2);
    % Update Ud
    tmp_Ud = Y * (W .*bsxfun(@minus, X, mu))';
    for iD = 1:D
        Ud(iD, :) = ( (bsxfun(@times, Y, W(iD, :)) * Y') \ tmp_Ud(:, iD) )';
%         Ud(iD, :) = ( (Y * diag(W(iD, :)) * Y') \ tmp_Ud(:, iD) )';
    end
    [Ud, ~] = qr(Ud, 0);
    % Update Y
    tmp_Y = Ud' * (W .*bsxfun(@minus, X, mu));
    for iN = 1:N
        Y(:, iN) = (Ud' * bsxfun(@times, W(:, iN), Ud)) \ tmp_Y(:, iN);
%         Y(:, iN) = (Ud' * diag(W(:, iN)) * Ud) \ tmp_Y(:, iN);
    end
    % Check convergence
    error = max(max(abs( X0 - bsxfun(@plus, mu, Ud*Y ))));
%     fprintf('Iteration %d, error is %f\n', iter, error);
    if error < thr || iter > maxIter
        break;
    end
    X0 = bsxfun(@plus, mu, Ud*Y);
    iter = iter + 1;
    
end
fprintf('Iteration %d, error is %f\n', iter, error);
mu = mu + Ud * sum(Y, 2)/N;
Y = bsxfun(@minus, Y, sum(Y, 2)/N);

end