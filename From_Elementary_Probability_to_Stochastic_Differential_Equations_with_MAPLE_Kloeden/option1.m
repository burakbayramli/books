%OPTION1  Stochastic Euler method on stochastic volatility SDE
%
% SDE is  dX(1) = lambda*X(1) dt + X(2)*sqrt(X(1)) dW(1),     X(1)_0 = Xzero(1)
%         dX(2) = (sigma_0-X(2)) dt + sqrt(X(2)) dW(2),       X(2)_0 = sigma_0
%
%   X(1) is the asset price, X(2) represents the volatility
%
% European Option Price
% Non-vectorized 

clf
randn('state',1)
T = 1; N = 2^8; Delta = T/N; M = 1e+4;            
lambda = 0.05; Xzero = 1; sigma_zero = 0.8; r = 0.05;

Pricesum = 0;
for s = 1:M
    Xem1 = Xzero;
    Xem2 = sigma_zero;
    for j = 1:N
        Winc1 = sqrt(Delta)*randn;
        Winc2 = sqrt(Delta)*randn;
        Xem1 = abs(Xem1 + Delta*lambda*Xem1 + sqrt(Xem1)*Xem2*Winc1);
        Xem2 = abs(Xem2 + Delta*(sigma_zero - Xem2) + sqrt(Xem2)*Winc2);
    end
Pricesum = Pricesum + max(0,Xem1-1);
end

Price = exp(-r)*Pricesum/M
