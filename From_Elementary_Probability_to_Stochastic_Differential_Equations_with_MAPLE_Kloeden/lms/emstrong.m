%EMSTRONG  Test strong convergence of Euler-Maruyama
%
% Solves    dX = lambda*X dt + mu*X dW,   X(0) = Xzero,
%         where lambda = 2, mu = 1 and Xzer0 = 1.
%
% Discretized Brownian path over [0,1] has dt = 2^(-9).
% E-M uses 5 different timesteps: 16dt, 8dt, 4dt, 2dt, dt.
% Examine strong convergence at T=1:  E | X_L - X(T) |.

clf
randn('state',100)                % set the state of randn
lambda = 2; mu = 1; Xzero = 1;    % problem parameters
T = 1; N = 2^9; dt = T/N;         %
M = 1000;                         % number of paths sampled

Xerr = zeros(M,5);                % preallocate array
for s = 1:M,                      % sample over discrete Brownian paths
    dW = sqrt(dt)*randn(1,N);     % Brownian increments
    W = cumsum(dW);               % discrete Brownian path 
    Xtrue = Xzero*exp((lambda-0.5*mu^2)+mu*W(end));
    for p = 1:5                            
        R = 2^(p-1); Dt = R*dt; L = N/R;     % L Euler steps of size Dt = R*dt
        Xtemp = Xzero;
        for j = 1:L
             Winc = sum(dW(R*(j-1)+1:R*j));
             Xtemp = Xtemp + Dt*lambda*Xtemp + mu*Xtemp*Winc;
        end
        Xerr(s,p) = abs(Xtemp - Xtrue);      % store the error at t = 1
    end
end

Dtvals = dt*(2.^([0:4]));               
loglog(Dtvals,mean(Xerr),'b*-'), hold on
loglog(Dtvals,(Dtvals.^(.5)),'r--'), hold off % reference slope of 1/2 
axis([1e-3 1e-1 1e-4 1])
xlabel('\Delta t'), ylabel('Sample average of | X(T) - X_L |')
title('emstrong.m','FontSize',10)

%%%% Least squares fit of error = C * Dt^q %%%%
A = [ones(5,1), log(Dtvals)']; rhs = log(mean(Xerr)');
sol = A\rhs; q = sol(2)
resid = norm(A*sol - rhs)


