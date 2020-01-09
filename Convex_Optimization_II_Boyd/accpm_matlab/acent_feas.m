function [x_star,H,niter] = acent_feas(A,b,x_0) 
% ACENT_FEAS computes the analytical center of the linear inequalities 
%            {Ax <= b}, i.e., the solution to 
%               minimize    - sum_{i=1,...,m}log(b_i - a_i^Tx) 
%            x0 is a strictly feasible starting point  for Newton's method. 
%
% Written by Joelle Skaf 

%algorithm parameters
ALPHA = 0.01;
BETA = 0.5;
EPSILON = 1e-6;
MAXITERS = 20;

if (min(b- A*x_0) <= 0) % x0 not feasible
    fprintf('FAILED');
    nu_star = []; x_star = []; lambda_hist=[];
    return;
end

m = length(b);
n = length(x_0);

x = x_0; lambda_hist = [];
for iter=1:MAXITERS
    s = b - A*x; 
    H = A'*diag(s.^(-2))*A;
    g = A'*(s.^(-1));    
    dx = -H\g; 
    lambdasqr = -g'*dx;
    if lambdasqr/2 <= EPSILON break; end

    % backtracking line search
    % first bring the point inside the domain
    t=1; 
    while min(b - A*(x+t*dx))<=0 t = BETA*t; end
    % now do backtracking line search
    while -sum(log(b - A*(x+t*dx)))+sum(log(b-A*x))-ALPHA*t*g'*dx> 0
        t = BETA*t;
    end
    x = x + t*dx;
end
niter = iter; 
if iter==MAXITERS % MAXITERS reached
    fprintf('ERROR: MAXITERS reached.\n');
    x_star=[]; 
else
    x_star = x;
end
