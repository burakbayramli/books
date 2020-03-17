function [mu, Ud, Y, flag] = pf(X, d, W)
% Function [mu,Ud,Y]=pf(X,d,W)
% Parameters
% X: D by N data matrix.
% d: Number of principal components.
% W: D by N binary matrix denoting known (1) or missing (0) entries
% Returned values
% mu: Mean of the data.
% Ud: Orthonormal basis for the subspace.
% Y: Low-dimensional representation (or principal components).
% Description
% Finds the d principal components of a set of points from the data X with incomplete entries as specified in
% W using the Power Factorization algorithm.

% Set parameters
[D, N] = size(X);
thr = 1e-10;
maxIter = 15;
iter = 1;
% Initialization
Ud = randn(D, d); 
[Ud, ~] = qr(Ud, 0);
Y = randn(d, N);
mu = randn(D, 1);
% Check validity
flag = 0;
if (min( sum(W, 1) ) < d) || (min( sum(W, 2) ) < d)
    flag = 1; % method cannot be applied in this case.
    return;
end
% PF
X0 = bsxfun(@plus, mu, Ud*Y);
while(1)
    % Update mu
    mu = sum( W .* (X - Ud * Y),  2) ./ sum(W, 2);
    % Update Ud
    tmp_Ud = Y * (W .*bsxfun(@minus, X, mu))';
    for iD = 1:D
%         Ud(iD, :) = ( (Y * diag(W(iD, :)) * Y') \ tmp_Ud(:, iD) )';
        Y_mask = Y(:, W(iD, :) == 1);
        Ud(iD, :) = ( ( Y_mask * Y_mask') \ tmp_Ud(:, iD) )';
    end
    [Ud, ~] = qr(Ud, 0);
    % Update Y
    tmp_Y = Ud' * (W .*bsxfun(@minus, X, mu));
    for iN = 1:N
%         Y(:, iN) = (Ud' * diag(W(:, iN)) * Ud) \ tmp_Y(:, iN);
        Ud_mask = Ud(W(:, iN)==1, :);
        Y(:, iN) = (Ud_mask' * Ud_mask) \ tmp_Y(:, iN);
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
