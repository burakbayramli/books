% AMERICAN  Binomial method for an American Put.
%
% Vectorized version, based on euro5.m

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%
S = 9; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Re-usable computations
dpowers = d.^([M:-1:0]');
upowers = u.^([0:M]');
scale1 = p*exp(-r*dt);
scale2 = (1-p)*exp(-r*dt);

% Option values at time T
W = max(E-S*dpowers.*upowers,0);

% Re-trace to get option value at time zero
for i = M:-1:1
    Si = S*dpowers(M-i+2:M+1).*upowers(1:i);
    W = max(max(E-Si,0),scale1*W(2:i+1) + scale2*W(1:i));
end

disp('Option value is'), disp(W)
