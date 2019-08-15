function [sigma,ecode] = sigini(funfcn,c1,c2,x,d,IB,Eps,Parmeter);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Calculates sigma0 in gradient-projection method
% Side conditions g(x) >= 0, h(x) = 0
% FUNCTIONS:
% func_f.m, grad_f.m, func_g.m, grad_g.m, func_h.m, grad_h.m
% restor.m
% INPUT  sigma  current step width
%        x      current state vector
%        d      current descend direction
%        IB     Index of active inequalities
%        nr     Number of example
%        Eps    Quantity of tolerance
%        c1     lower bound for sigma
%        c2     upper bound for for sigma
% OUTPUT sigma  improved step width

sigma0 = 1; ecode = 0;
[x1,sigma,ecode] = restor(funfcn,sigma0,x,d,IB,Eps,Parmeter);
f      = feval(funfcn,x,1,Parmeter);
f1     = feval(funfcn,x1,1,Parmeter);
gradf  = feval(funfcn,x,4,Parmeter);
sp     = gradf*d;
nn     = f1 - f + sp;
sigma1 = min(sp/(2*nn),c2);
sigma  = min(max(c1,sigma1),sigma);
[u,sigma,ecode] = restor(funfcn,sigma,x,d,IB,Eps,Parmeter);
g      = feval(funfcn,u,2,Parmeter);
h      = feval(funfcn,u,3,Parmeter);
ming   = min(g);
minh   = min(abs(h));
if ((ming < - Eps) | (minh > Eps)) & (sigma > c1)
   sigma = c1;
end;
