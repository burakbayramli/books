function [x_star, H, niter] = acent(A,b,x_0) 
% ACENT computes the analytical center of the linear inequalities 
%       {Ax <= b}, i.e., the solution to 
%           minimize    - sum_{i=1,...,m}log(b_i - a_i^Tx) 
%       using the infeasible start Newton method, i.e. solving 
%           minimize    -sum_{i-1,...,m}log(y_i) 
%               s.t.    y = b - Ax 
%       where we initialize the procedure with y_0 > 0. 
%       The initial point x_0 need not be strictly inside the 
%       polyhedron {x | Ax <= b}. 
%
% Written by Joelle Skaf 

MAXITERS = 50;
ALPHA = 0.1;
BETA  = 0.5;
TOL = 1e-6;

%
% INFEASIBLE NEWTON 
%

m = length(b);
n = length(x_0);

% starting point 
e = 0.01;
y = pos(A*x_0 - b + e); 
y(find(y==0)) = 1;
x = x_0; 
w = zeros(m,1);                 % dual variable

for iters = 1:MAXITERS 
    g = -1./y;
    H = diag(1./(y.^2)); 
    rd = g + w; 
    rp = A*x + y - b;
    res = [A'*w;rd;rp];
    
    if (norm(res) < sqrt(TOL)), break; end;
    
    dx = (A'*H*A)\(A'*(g - H*rp)); 
    dy = - A*dx - rp;
    dw = - H*dy - rd; 

    t = 1;
    while(min(y+t*dy) <= 0) 
        t = BETA*t;
    end
    newres = [A'*(w+t*dw); w + t*dw - 1./(y+t*dy); ...
              A*(x+t*dx) + y + t*dy - b];
    while (norm(newres) > (1-ALPHA*t)*norm(res)) 
        t = BETA*t; 
        newres = [A'*(w+t*dw); w + t*dw - 1./(y+t*dy); ...
                  A*(x+t*dx) + y + t*dy - b];
    end
    x = x + t*dx; y = y + t*dy; w = w + t*dw;
end
x_star = x; 
H = A'*diag((b-A*x).^(-2))*A;
niter = iters - 1;              % total number of newton steps taken
