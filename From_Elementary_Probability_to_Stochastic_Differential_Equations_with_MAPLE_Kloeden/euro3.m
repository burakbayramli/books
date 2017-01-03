% EURO3  Binomial method for a European Put.
%
% Partially vectorized version.

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Option values at time T
W = max(E-S*d.^([M:-1:0]').*u.^([0:M]'),0);

% Re-trace to get option value at time zero
for i = M:-1:1
    for n = 1:i
        W(n) = exp(-r*dt)*(p*W(n+1) + (1-p)*W(n));
    end
end

disp('Option value is'), disp(W(1))
