%EULER2  Stochastic Euler method on square root process SDE
%
% SDE is  dX = lambda*X dt + sigma*sqrt(X) dW,   X(0) = Xzero.
%
% Discretized Brownian path over [0,1] has delta = 2^(-7).
% Method uses timestep R*delta, for R = 1,2,4,8, over a single path.

clf
randn('state',1)
T = 1; N = 2^7; delta = T/N;         
lambda = 0.05; sigma = 0.8; Xzero = 1; 
dW = sqrt(delta)*randn(1,N);   

linetypes ={'bl:+','g-.o','r--x','m-*'};
for k = 1:4
    R = 2^(k-1);
    Delta = R*delta; L = N/R;    
    Xem = zeros(1,L+1);      
    Xem(1) = Xzero;
    for j = 1:L
        Winc = sum(dW(R*(j-1)+1:R*j)); 
        Xem(j+1) = abs(Xem(j) + Delta*lambda*Xem(j) + sigma*sqrt(Xem(j))*Winc);
    end
    plot([0:Delta:T],Xem,linetypes{k}), hold on
end
legend('\Delta = 2^{-7}','\Delta = 2^{-6}','\Delta = 2^{-5}','\Delta = 2^{-4}')
xlabel('t','FontSize',16), ylabel('X','FontSize',16)

