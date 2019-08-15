function [u,sigma,ecode] = restor(funfcn,sigma,x,d,IB,Eps,Parmeter)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Restoration for nonlinear gradient-projection method
% cf. Spellucci p. 348
% FUNCTIONS:
% func_g.m, grad_g.m, func_h.m, grad_h.m
% INPUT  sigma  Current step number
%        x      Current state vector
%        d      Current descend direction
%        IB     Index of active inequalities
%        nr     Number of example
%        Eps    Quantity of tolerance
% OUTPUT u      Improved state vector
%        sigma  Improved step width

ecode = 0; maxit = 10; Eps = Eps/100; q = length(IB);
if q == 0
   gradh = feval(funfcn,x,6,Parmeter);
   [p,n] = size(gradh);
   [Q,R] = qr(gradh');
else
   gradg = feval(funfcn,x,5,Parmeter);
   gradh = feval(funfcn,x,6,Parmeter);
   [m,n] = size(gradg);
   [p,n] = size(gradh);
   NB    = gradg(IB,:);
   N     = [NB; gradh];
   [Q,R] = qr(N');
end
S = R(1:q+p,:)'; sigma = 2*sigma; null = zeros(n-q-p,1);
k = 0; done_R = 0;
while ~done_R,
   k = k + 1; sigma = sigma/2; c = zeros(q+p,1);
   i = 0; done = 0; error  = 0;
   while ~done,
      i  = i+1;
      u  = x - sigma*d + Q*[c;null];
      F1 = feval(funfcn,u,2,Parmeter);
      H1 = feval(funfcn,u,3,Parmeter);
      F1 = [F1(IB); H1];
      w  = S\F1;
      c  = c - w;
      normc = norm(c);
      if (normc > 1.0E4) | (i > 10), error = 1; end
      normw = norm(w);
      done  = (normw < Eps) | (error == 1);
   end
   if error == 1, disp('sigma reduced in restoration '); end
   done_R = (normw < Eps) | (k > maxit);
end
if k > maxit, disp(' Restoration fails '); ecode = 1; end
