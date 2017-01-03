% EURO6  Binomial method for a European Put.
%
% Uses explicit solution based on binomial expansion.
% Unvectorized, subject to overflow.

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%%
S = 5; E = 10; T = 1; r = 0.06; sigma = 0.3; M = 256; 
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));   
u = A + sqrt(A^2-1); d = 1/u; p = (exp(r*dt)-d)/(u-d); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Option values at time T
W = max(E-S*d.^([M:-1:0]').*u.^([0:M]'),0);

% Binomial coefficients 
v = zeros(M+1,1);
for k = 1:M+1
    v(k) = nchoosek(M,k-1);
end

value  = exp(-r*T)*sum(v.*(p.^([0:M]')).*((1-p).^([M:-1:0]')).*W)';
disp('Option value is'), disp(value)
