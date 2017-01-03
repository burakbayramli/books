%EMWEAK  Test weak convergence of Euler-Maruyama
%
% Solves    dX = lambda*X dt + mu*X dW,   X(0) = Xzero,
%        where lambda = 2, mu = 1 and Xzer0 = 1.
%
% E-M uses 5 different timesteps: 2^(p-10),  p = 1,2,3,4,5.
% Examine weak convergence at T=1:   | E (X_L) - E (X(T)) |.
%
% Different paths are used for each E-M timestep.
% Code is vectorized over paths.
%
% Uncommenting the line indicated below gives the weak E-M method.

clf
randn('state',100);                      % set the state of randn                   
lambda = 2; mu = 0.1; Xzero = 1; T = 1;  % problem parameters
M = 5e4;                                 % number of paths sampled

Xem = zeros(5,1);                        % preallocate arrays
for p = 1:5                              % take various Euler timesteps
       Dt = 2^(p-10); L = T/Dt;          % L Euler steps of size Dt 
       Xtemp = Xzero*ones(M,1);            
       for j = 1:L
           Winc = sqrt(Dt)*randn(M,1);
           % Winc = sqrt(Dt)*sign(randn(M,1)); %% use for weak E-M %%
           Xtemp = Xtemp + Dt*lambda*Xtemp + mu*Xtemp.*Winc;
       end
       Xem(p) = mean(Xtemp);
end
Xerr = abs(Xem - exp(lambda));

Dtvals = 2.^([1:5]-10);          

loglog(Dtvals,Xerr,'b*-'), hold on
loglog(Dtvals,Dtvals,'r--'), hold off    % reference slope of 1
axis([1e-3 1e-1 1e-4 1])
xlabel('\Delta t'), ylabel('| E(X(T)) - Sample average of X_L |')
title('emweak.m','FontSize',10)

%%%% Least squares fit of error = C * Dt^q %%%%
A = [ones(p,1), log(Dtvals)']; rhs = log(Xerr);
sol = A\rhs; q = sol(2)
resid = norm(A*sol - rhs)
