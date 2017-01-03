%BINOM  Binomial method for a European call
%

%%%%%%%%%% Problem and method parameters %%%%%%%%%%%%
S = 2; E = 1; r = 0.05; sigma = 0.25; T = 3; M = 256;
dt = T/M; A = 0.5*(exp(-r*dt)+exp((r+sigma^2)*dt));
d = A - sqrt(A^2-1); u = A + sqrt(A^2-1);
p = (exp(r*dt)-d)/(u-d);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Option values at time T
W = max(S*d.^([M:-1:0]').*u.^([0:M]')-E,0);

B = (1-p)*eye(M+1,M+1) + p*diag(ones(M,1),1);
B = sparse(B);
% Re-trace to get option value at time zero
for i = M:-1:1
    W = B(1:i,1:i+1)*W;
end
W = exp(-r*T)*W;
