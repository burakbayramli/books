% function [Y,cost] = svt(X,Omega,tau,delta)
% 
% SVT completes a low-rank matrix X with missing entries in Omega complement
% by solving the following optimization problem
%
% min_{Y} 1/2 |Y|_F^2 + tau*|Y|_* subject to P_{Omega}(Y) = P_{Omega}(X)
%
% This implementation uses the Singular Value Thresholding (SVT) algorithm
%
% min_{Y} 1/2 |Y|_F^2 + tau*|Y|_* + <Z, P_{Omega}(X-Y)>
%
% Copyright @ Rene Vidal
% Stanford, November 2012

function [Y,cost] = svt(X,Omega,tau,delta)

normX = norm(X);
cost = normX;

if nargin < 3
    tau = 5*sqrt(prod(size(X)));
    delta = min(2,prod(size(X))/sum(sum(Omega)));% see equation 5.1 in http://arxiv.org/pdf/0810.3286.pdf 
end

i = 1;
maxiter = 200;
Z = Omega.*X;%zeros(size(X));

while( i<maxiter && cost(i)>1e-10*normX)
    i = i + 1;
    Y = mregularize(Z,'nuclear',tau);
    Z = Z + delta*(Omega.*(X-Y));
    cost(i) = norm(X-Y);%1/2*norm(Y,'fro')^2 + tau*sum(svd(Y));
end
fprintf('Iteration: %d, residual: %f\n', i, cost(i))