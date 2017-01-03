% EURO9  Binomial method for a European Put.
%
% Uses explicit solution based on binomial expansion.
% Vectorized, based on logs to avoid overflow, 
% and avoids computing with zeros.

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cut-off index
z = max(1,min(M+1,floor( log((E*u)/(S*d^(M+1)))/(log(u/d)) )));

% Option values at time T
W = E - S*d.^([M:-1:M-z+1]').*u.^([0:z-1]');

% log/cumsum version using cut-off index z
tmp1 = cumsum(log([1;[M:-1:M-z+2]'])) - cumsum(log([1;[1:z-1]']));
tmp2 = tmp1 + log(p)*([0:z-1]') + log(1-p)*([M:-1:M-z+1]');

value = exp(-r*T)*sum(exp(tmp2).*W);

disp('Option value is'), disp(value)
