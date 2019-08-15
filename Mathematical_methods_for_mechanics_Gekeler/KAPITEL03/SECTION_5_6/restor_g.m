function [u,sigma,ecode] = restor(funfcn,sigma,x,d,IB,Eps,Parmeter);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Restoration for nonlinear gradient-projection method
% cf. Spellucci p. 348

ecode = 0; maxit = 30; Eps = Eps/100; q = length(IB);
if q == 0
   u = x - sigma*d;
else
   gradg = feval(funfcn,x,5,Parmeter);
   [m,n] = size(gradg);
   N     = gradg(IB,:)';
   [Q,R] = qr(N);
   S     = R(1:q,:)';
   sigma = 2*sigma; null = zeros(n-q,1);
   k = 0; done_R = 0;
   while ~done_R,
      k = k + 1;
      sigma = sigma/2;
      c = zeros(q,1);
      i = 0; done = 0; error = 0;
      while ~done,
         i  = i+1;
         u  = x - sigma*d + Q*[c;null];
         F1 = feval(funfcn,u,2,Parmeter);
         F1 = F1(IB);
         w  = S\F1;
         c  = c - w;
         normc = norm(c);
         if (normc > 1.0E4) | (i > 15), error = 1; end
         normw = norm(w);
         done = (normw < Eps) | (error == 1);
      end;
      if (i > 10) | (error == 1), disp('sigma reduced in restor ');
      end
      done_R = (normw < Eps) | (k > maxit);
   end
   if (k > maxit), disp(' Restoration fails '); ecode = 1; end
end
