% function X = mregularize(Y,method,tau)
% 
% mregularize finds the matrix X that is closest in Frobenious norm
% to a given matrix Y using different regularization methods
%
% min_X   1/2 * | Y - X |_F^2 + tau * Omega(X)
% 
% Omega(X) = |X|_F^2/2      (method = 'fro')
% Omega(X) = |X|_1          (method = 'L1')
% Omega(X) = |X|_{21}       (method = 'L21')
% Omega(X) = |X|_{12}       (method = 'L12')
% Omega(X) = |X|_*          (method = 'nuclear')
% Omega(X) = |X|_*+|X|_{21} (method = 'mixed')
%
% Copyright @ Rene Vidal
% Stanford, November 2012

function [X,cost] = mregularize(Y,method,tau)

if nargin < 3
    tau = 100/norm(Y);
end

if strcmp(method,'fro')
    X = Y / (1+tau);
    cost = 1/2*norm(Y-X,'fro')^2 + tau*norm(X,'fro')^2;

elseif strcmp(method,'L1')
    X = (abs(Y) - tau);
    X = (X > 0).* X .* sign(Y);
    cost = 1/2*norm(Y-X,'fro')^2 + tau*sum(sum(abs(X)));

elseif strcmp(method,'L21')
    [Yn,cnormY] = cnormalize(Y);
    Y = Y - tau * Yn;
    X = ones(size(Y,1),1)*(cnormY > tau);
    X = X.*Y;
    cost = 1/2*norm(Y-X,'fro')^2 + tau*sum(sqrt(sum(conj(X).*X,1)));

elseif strcmp(method,'L12')
    [X,cost] = mregularize(Y','L21',tau)';

elseif strcmp(method,'nuclear')
    [U,S,V] = svd(Y,'econ');
    s = max(diag(S)-tau,0);
    X = U * diag(s) * V';
    cost = 1/2*norm(Y-X,'fro')^2 + tau*sum(s);

elseif strcmp(method,'mixed')
    X = mregularize(Y,'L21');
    for i=1:100
        [U,S,V] = svd(X,'econ');
        r = sum(diag(S)>tau);
        X = mregularize(U(:,1:r)*(U(:,1:r)'*Y),'L21');
        %cost(i) = norm(X(:,1:100)-Y(:,1:100));
        cost(i) = norm(X(:,101:200)-Y(:,101:200));
    end

elseif strcmp(method,'mixednuclearL21')
    alpha = 10/norm(Y);
    lambda1 = tau;
    lambda2 = tau/sqrt(prod(size(Y)));
    Z1 = zeros(size(Y));
    Z2 = zeros(size(Y));
    X  = mregularize(Y,'nuclear',tau);
    for i=1:100
        X1 = mregularize(X + Z1/alpha,'nuclear',lambda1/alpha);
        X2 = mregularize(X + Z2/alpha,'L21',lambda2/alpha);
        X = (Y + alpha*X1 + alpha*X2 - Z1 - Z2)/(1+2*alpha);
        Z1 = Z1 + alpha*(X-X1);
        Z2 = Z2 + alpha*(X-X2);
        cost(i,1) = 1/2*norm(Y-X,'fro')^2 + lambda1*sum(svd(X)) + lambda2*sum(sqrt(sum(conj(X).*X,1)));
    %[norm(X-X1) norm(X-X2)]
    end
end