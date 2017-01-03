%Euler1  Stochastic Euler method on square root process SDE
%
% SDE is  dX = lambda*X dt + sigma*sqrt(X) dW,   X(0) = Xzero.
% Method uses timestep of Delta = 2^(-8) over a single path.

clf
randn('state',1)
T = 1; N = 2^8; Delta = T/N;           
lambda = 0.05; sigma = 0.8; Xzero = 1;  

Xem = zeros(1,N+1);    
Xem(1) = Xzero;
for j = 1:N
    Winc = sqrt(Delta)*randn;
    Xem(j+1) = abs(Xem(j) + Delta*lambda*Xem(j) + sigma*sqrt(Xem(j))*Winc);
end

plot([0:Delta:T],Xem,'r--') 
xlabel('t','FontSize',16), ylabel('X','FontSize',16)
