function [x,f,errorcode,PFAD] = gp_g(funfcn,x,Pargpv,Parmeter)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Gradient-projection method
% with nonlinear side conditions
% f(x) = Min!, g(x) >= 0
% FUNCTIONS: restor.m,sigini.m
% Set lin_restr = 1 in linear restrictions
% else slow convergence
% INPUT  funfcn: Name of problem
%        x: FEASIBLE point as start vector
%        lin_restr = 0/1, 1 in  lin. restrictions
%        Tol   = Tolerance
%        Maxit = max. step number 
% OUTPUT (x,y) Kuhn-Tucker point in case of convergence
%        f = f(x);
%        errorcode = 1: Max. step number in iteration
%        errorcode = 2: Max. step number in backtracking
%        errorcode = 3: x not feasible

% Parameter --------------------
Accel = Pargpv(1); Eps = Pargpv(2); Gamma = Pargpv(3);
Maxit = Pargpv(4); Lin_restr = Pargpv(5); Tol = Pargpv(6);
done = 0; PFAD = [x];
% -- PARAMETER:  -----------------------------------
Geps  = 1.0E-5; % instead zero
GAmax = 40;     % bound for GA-test
beta  = 0.5;    % for GA-Test, Spellucci, S. 353
delta = 0.01;   % for GA-Test
c1    = 0.05;   % for sigma0
c2    = 2;      % for sigma0
% --- Initialization -------------------------------
done = 0; dnorm = 1; errorcode = 0; ecode = 0;
it =  0; iteration = 0; minw = -1;
omega = 0;  % omega = 0, if incativation allowed
theta = 0;  % theta = 1, if inactivation applicable
N = feval(funfcn,x,5,Parmeter);
[m,n] = size(N);
H = eye(n);
% -- Check feasibility of x -------------------------
g  = feval(funfcn,x,2,Parmeter);
IB = find(abs(g) <= Eps);
if min(g) < - Eps
   disp('Start vector not feasible ');
   errorcode = 3;
end
Q1 = H;
if length(IB) > 0
   N = feval(funfcn,x,5,Parmeter);
   N = N(IB,:)';
   [Q1,R] = qr(N);
end
% - Iteration loop -----------------------------
while ~done
   disp('--------------------------------------');
   it    = it + 1;
   gradf = feval(funfcn,x,4,Parmeter);
   gradf = gradf';
   % - Solve linear system of equations ----------
   q = length(IB);
   if q > n
      IB = IB(1:n); q  = n;
   end
   if q == 0
      d     = H\gradf;
      w     = 0;
      minw  = 0;
      y     = zeros(m,1);
      gradl = gradf';
    %  Q1   = eye(n);
   else
      N    = feval(funfcn,x,5,Parmeter);
      N    = N(IB,:)';          % do not transpose, see below!
      D    = [H  N; N'  zeros(q,q)];
      v    = [gradf; zeros(q,1)];
      u    = D\v;
      d    = u(1:n);
      w    = u(n+1:n+q);
      minw = min(w);
      y     = zeros(m,1);
      y(IB) = w;
      gradl = gradf' - y(IB)'*N';
      %  [Q1,R] = qr(N);          bringt nichts in B1
   end
   dnorm = norm(d);
   if theta == 0
      iteration = 0;  % check inactivation
   else
      iteration = 1;  % normal step of iteration
   end
   if iteration == 0
      % - Check wether inactivation makes sense ----------
      if minw < 0
         [Q,R] = qr(N);
         u = Q'*gradf;
         if q == n
            normu = 0;
         else
            normu = norm(u(q+1:n));
         end
         if -minw > Gamma*normu
            theta = 1;         % Inactivation makes sense
         end
      end
      % - Perform inactivation if applicable ----
      if ((theta == 1) & (omega == 0)) ...
                      | ((omega == 1) & (dnorm < Eps))
         disp(' step of inactivation ');
         t = min(find(w == min(w)))
         K = find(IB ~= IB(t));
         if length(K) == 0
            IB = []
         else
            IB = IB(K)
         end
         theta = 1;
         disp(' Inactivation applicable ');
      else
         iteration = 1;
      end
   end
   % -- the normal step of iteration -----------------
   if (iteration == 1) & (dnorm > Geps)
      disp('normal step of iteration ');
      if Lin_restr == 1
         IN = 1:m;
         g  = feval(funfcn,x,2,Parmeter);
         u  = feval(funfcn,x,5,Parmeter);
         u  = u*d;
         IN(IB) = zeros(1,q);
         IN = IN(find(IN > 0));
         g  = g(IN);
         u  = u(IN);
         L  = find(u > 0);
         if length(L) > 0
            sigma0 = min(g(L)./u(L));
         else
            sigma0 = 1;
         end
      else
         % -- Restoration and Goldstein-Armijo-Test ---
         [sigma0,ecode] = sigini_g(funfcn,c1,c2,x,d,IB,Eps,Parmeter);
      end
      if ecode > 0, disp(' sigini fails '); end
      f     = feval(funfcn,x,1,Parmeter);
      gradf = feval(funfcn,x,4,Parmeter);
      l = 0; done_GA = 0;
      if ecode > 0, done_GA = 1; u = x; end
      sigma = sigma0/beta;
      while ~done_GA,
         done1 = 0;
         done2 = 0;
         l = l + 1;
         sigma = sigma*beta;
         if Lin_restr == 0
            [u,sigma,ecode] = restor_g(funfcn,sigma,x,d,IB,Eps,Parmeter);
            s = u - x;
         else
            s = - sigma*d;
            u = x + s;
         end
         norms   = norm(s);
         g       = feval(funfcn,u,2,Parmeter);
         done1   = (min(g) > - Geps);%u zulaessig?,vgl.Bsp.1
         f1      = feval(funfcn,u,1,Parmeter);
         rs      = delta*sigma*gradf*d;
         done2   = (f - f1 >= rs);    % Abstieg hinreichend?
         done_GA = (done1 & done2) | (l > GAmax) | (norms < Eps);
         % done_GA = (done1 & done2) | (l > GAmax);
      end
      x = u;
      monitor = 1;
      if monitor == 1
         ITERGA_sigma_dnorm_IB = [l;sigma;dnorm;IB]'
         %XXX = [0,-d(1)];
         %YYY = [0,-d(2)];
         %plot(XXX,YYY,'k'), hold on
         %pause
      end
      if (l > GAmax) | (ecode > 0), errorcode = 2; end
      if l > GAmax, disp(' Backtracking fails! '); end
      %-- Anti-Zigzag-Strategy ---------------------
      g     = feval(funfcn,x,2,Parmeter);
      IB1   = find(abs(g) < Eps);
      q     = length(IB1);
      omega = 0;
      for k = 1:q
         if length(find(IB == IB1(k))) == 0
            omega = 1;
         end
      end
      IBA   = IB;
      IB    = IB1;
      theta = 0;
      % Adaption of H --------------------------------
      gradf1 = feval(funfcn,x,4,Parmeter);
      gradg1 = feval(funfcn,x,5,Parmeter);
      gradl1 = gradf1 - y'*gradg1;
      % -- Anpassung von H
      if Accel == 1        % Method of Rosen
         H = eye(n);
      end
      if Accel == 2            % concex problems
         q  = length(IBA);
         Q2 = eye(n);
         if length(IB) > 0
            N      = gradg1(IB,:)';
            [Q2,R] = qr(N);
         end
         H1 = Q1'*H*Q1;
         H3 = zeros(n,n);
         if q < n
            E  = zeros(n-q,n-q);
            C  = H1(q+1:n,q+1:n);
            s1 =  Q1'*s;
            s1 = s1(q+1:n);
            w  = Q1'*(gradl1 - gradl)';
            w  = w(q+1:n);
            v  = C*s1;
            n1 = v'*s1;
            n2 = w'*s1;
            if (abs(n1) > 0) & (n2 > 0)
               E = - v*v'/n1 + w*w'/n2;
            end
            if q == 0
               H3 = E;
            else
               H3 = [zeros(q,n); zeros(n-q,q) E];
            end
         end
         H4 = H1 + H3;
         H  = Q2*H4*Q2';
         Q1 = Q2;
      end
      if Accel == 3        % May help in nonconvex problems
         v  = gradl1 - gradl;
         w  = H*s;
         n1 = s'*w;
         n2 = v*s;
         if n2 >= 0.2*n1
            %            cc = 1;
         else
            %            cc = 0.8*n1/(n1 - n2);
            %          end;
            %          v  = cc*v + (1 - cc)*w';
            n2 = v*s;
            if (abs(n1) > Eps) & (abs(n2) > Eps)
               H = H - w*w'/n1 + v'*v/n2
            end
         end
      end
      %----------------------------------------------
   end         % End of normal step of iteration
   PFAD = [PFAD x];
   f    = feval(funfcn,x,1,Parmeter);
   if it > Maxit, errorcode = 1; end
   done   = ((dnorm < Tol) & (minw > - Eps)) | (errorcode > 0);
   it_x_f = [it;x;f]'
end
