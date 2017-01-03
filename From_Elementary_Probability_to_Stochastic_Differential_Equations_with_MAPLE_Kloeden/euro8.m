% EURO8  Binomial method for a European Put.
%
% Uses explicit solution based on binomial expansion.
% Vectorized and based on logs to avoid overflow.

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Option values at time T
W = max(E-S*d.^([M:-1:0]').*u.^([0:M]'),0);

% log/cumsum version 
csl = cumsum(log([1;[1:M]']));
tmp = csl(M+1) - csl - csl(M+1:-1:1) + log(p)*([0:M]') + log(1-p)*([M:-1:0]');
value = exp(-r*T)*sum(exp(tmp).*W);

disp('Option value is'), disp(value)
