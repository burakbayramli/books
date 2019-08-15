function [x,f,errorcode] = gp(funfcn,x,Pargpv,Parmeter);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Gradient-projection method
% with nonlinear side conditions
% f(x) = Min!,  g(x) >= 0;  h(x) = 0
% f, g, h all not identical zero!
% FUNCTIONS: restor.m,sigini.m
% Set lin_restr = 1 in linear restrictions
% else slow convergence
% INPUT  funfcn: Name of problem
%        x: FEASIBLE point as start vector
%        lin_restr = 0/1, 1 in  lin. restrictions
%        Tol       = Tolerance
%        Maxit     = max. step number 
% OUTPUT (x,y) Kuhn-Tucker point in case of convergence
%        f = f(x);
%        errorcode = 1: Max. step number in iteration
%        errorcode = 2: Max. step number in backtracking
%        errorcode = 3: x not feasible
% Examples must have the form Y = examplename(X,flag,Parmeter);
% X state vector, Parmeter: additional parameters for problem
% flag = 1: objective function
% flag = 2: gradient of objective function
% flag = 3: inequality constraints
% flag = 4: gradient of inequalities
% flag = 5: equality constraints
% flag = 6: gradient of equalities
% optionally in 2-dimensional problems: 
% flag = 7: boundary of feasible domain 

% -- PARAMETER:  ----------------------------------
Accel = Pargpv(1); Eps = Pargpv(2); Gamma = Pargpv(3);
Maxit = Pargpv(4); Lin_restr = Pargpv(5); Tol = Pargpv(6);
f      = NaN;
GAmax  = 20;     % Bound in GA-test
Geps   = 1.0E-5; % instead zero
beta   = 0.5;    % for GA-test, Spellucci, p. 353
delta  = 0.01;   % for GA-Test
c1     = 0.05;   % for sigma0
c2     = 2;      % for sigma0
% --- Initialization ---------------------------------
done = 0; dnorm = 1; errorcode = 0; ecode = 0;
it = 0; iteration = 0; minw = -1;
omega = 0;  % omega = 0, if inactivation allowed
theta = 0;  % theta = 1, if inactivation applicable
NG    = feval(funfcn,x,5,Parmeter);
NH    = feval(funfcn,x,6,Parmeter);
[m,n] = size(NG);   % m inequalities as constraints
[p,n] = size(NH);   % p equalities as constraints
H1    = eye(n);     % H = I, Method of Rosen
% -- Check feasibility of x --------------------------
g    = feval(funfcn,x,2,Parmeter);                     % NB
h    = feval(funfcn,x,3,Parmeter);
IB   = find(abs(g) <= Eps);
ming = min(g);
minh = min(abs(h));
if (ming <= - Eps) | (minh > Eps), errorcode = 3; end;
% - Iteration loop ------------------------------------
if errorcode == 3, done = 1; end
while ~done
   disp('--------------------------------------');
   it    = it + 1;
   gradf = feval(funfcn,x,4,Parmeter);
   gradf = gradf';
   % - Solve linear system of equations ----------
   q = length(IB);
   if q > n, IB = IB(1:n); q = n;
   end;
   if q == 0
      minw   = 0;
      y  = zeros(m,1);
      NH = feval(funfcn,x,6,Parmeter)';
      D  = [H1  NH; NH' zeros(p,p)];
      v  = [gradf; zeros(p,1)];
      u  = D\v;
      d  = u(1:n);
      z  = u(n+1:n+p);
   else
      NG = feval(funfcn,x,5,Parmeter);
      NG = NG(IB,:)';        % not transposed, see below !
      NH = feval(funfcn,x,6,Parmeter)';
      D  = [H1  NG NH; NG'  zeros(q,q+p); NH' zeros(p,p+q)];
      v  = [gradf; zeros(p+q,1)];
      u  = D\v;
      d  = u(1:n); w = u(n+1:n+q); y = zeros(m,1);
      y(IB) = w;
      z     = u(n+q+1:n+q+p);
      minw  = min(w);
      minw  = minw(1);
      gradl = gradf' - y(IB)'*NG' - z'*NH';
   end
   dnorm = norm(d);
   if theta == 0
      iteration = 0;  % Check inactivation
   else
      iteration = 1;  % normal iteration step
   end
   %%% Modification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
   if length(IB) == 0, iteration = 1; end    
   if iteration == 0
      % - Check wether inactivation applicable ------------
      if minw < - Eps,
         [Q,R] = qr(NG);
         u1    = Q'*gradf;
         if q == n
            normu = 0;
         else
            normu = norm(u1(q+1:n));
         end
         if -minw > Gamma*normu
            theta = 1;            % Inactivation applicable
         end
      end
      % - Perform inactivation if applicable ---------------
      if ((theta == 1) & (omega == 0)) ...
            | dnorm <= Eps
%           | ((omega == 1) & (dnorm <= Eps))
         disp('Inactivation step ');
         t = min(find(w == min(w)))
         IB
         K = find(IB ~= IB(t));
         if length(K) == 0
            IB = []
         else
            IB = IB(K);
         end;
         theta = 1;
         disp(' Inactivation applicable ');
      else
         iteration = 1;
      end
   end
   % -- normal step of iteration --------------------
   if (iteration == 1) & (dnorm > Geps)
      disp('normal step of iteration ');
      if Lin_restr == 1
         IN     = 1:m;
         g      = feval(funfc,x,2,Parmeter);
         u      = feval(funfc,x,5,Parmeter)*d;
         IN(IB) = zeros(1,q);
         IN     = IN(find(IN > 0));
         g      = g(IN);
         u      = u(IN);
         L      = find(u > 0);
         if length(L) > 0
            sigma0 = min(g(L)./u(L));
            sigma0 = sigma0(1);
         else
            sigma0 = 1;
         end
      else
         % -- Restoration and Goldstein-Armijo-Test -------
         [sigma0,ecode] = sigini(funfcn,c1,c2,x,d,IB,Eps,Parmeter);
      end
      if ecode > 0, disp(' sigini fails '); end
      f     = feval(funfcn,x,1,Parmeter);
      gradf = feval(funfcn,x,4,Parmeter);
      l = 0; done_GA = 0;
      if ecode > 0, done_GA = 1; u = x; end;
      sigma = sigma0/beta;
      while ~done_GA,
         done1  = 0; done2  = 0; l = l + 1;
         sigma  = sigma*beta;
         if Lin_restr == 0
            [u,sigma,ecode] = restor(funfcn,sigma,x,d,IB,Eps,Parmeter);
            s = u - x;
         else
            s = - sigma*d; u = x + s;
         end
         norms  = norm(s);
         g      = feval(funfcn,u,2,Parmeter);
         done1  = (min(g) > - Eps);     % u feasible?
         f1     = feval(funfcn,u,1,Parmeter);
         rs     = delta*sigma*gradf*d;
         done2  = (f - f1 >= rs);    % descend sufficient?
         done_GA = (done1 & done2) | (l > GAmax) | (norms < Eps);
         %     done_GA = (done1 & done2) | (l > GAmax);
      end
      x  = u;
      disp(' GA-steps and sigma: ');
      l, sigma,
      dnorm;
      if (l > GAmax) | (ecode > 0), errorcode = 2; end
      %--- Anti-zigzagging-strategy -------------------
      g     = feval(funfcn,x,2,Parmeter);
      IB1   = find(abs(g) <= Eps);
      q     = length(IB1);
      omega = 1;
      if length(ismember(IB1,IB)) == length(IB1)
         disp(' Anti-zigzagging strategy ')
         omega = 0;
      end
      IB = IB1; theta = 0;
   end
   % it_x = [it,x.']
   f = feval(funfcn,x,1,Parmeter);
   if it > Maxit, errorcode =  1; end;
   done = ((dnorm < Tol) & (minw > - Eps)) | (errorcode > 0);
end
