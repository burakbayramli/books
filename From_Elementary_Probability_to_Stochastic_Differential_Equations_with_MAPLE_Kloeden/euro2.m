% EURO2  Binomial method for a European Put.
%
% Initial tree computation improved.

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

W = zeros(M+1,1);
% Asset prices at time T
for n = 1:M+1
    W(n) = S*(d^(M+1-n))*(u^(n-1));
end

% Option values at time T
for n = 1:M+1
    W(n) = max(E-W(n),0);
end

% Re-trace to get option value at time zero
for i = M:-1:1
    for n = 1:i
        W(n) = exp(-r*dt)*(p*W(n+1) + (1-p)*W(n));
    end
end

disp('Option value is'), disp(W(1))
