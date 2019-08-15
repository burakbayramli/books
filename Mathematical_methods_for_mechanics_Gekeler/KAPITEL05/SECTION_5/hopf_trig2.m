function [U,omga,mu,errorcode] = hopf_trig2(name,Parhopf,Parmeter);
% Hopf bifurcation with trig. collocation
% modified Newton method

errorcode = 0; F = [name]; n = Parhopf(1); tol = Parhopf(2);
maxit = Parhopf(3); Eps = Parhopf(4);
zeta1 = Parhopf(5); zeta2 = sqrt(1 - zeta1^2);
zeta1 = Eps*zeta1;  zeta2 = Eps*zeta2;
a = 2*pi/n; I = 1:n; NODES = a*I;
mu = []; X = [];
Y = feval(F,1,mu,X,Parmeter);
p    = size(Y,1);
A    = Y(:,1:p); B = Y(:,p+1:2*p);
Re_c = Y(:,2*p+1); Im_c = Y(:,2*p+2);
Re_d = Y(:,2*p+3); Im_d = Y(:,2*p+4);
omga0 = Y(1,2*p+5);
% -- discretization of exact eigensolutions --
U1 = Re_c*cos(NODES)  - Im_c*sin(NODES);
U2 = Im_c*cos(NODES)  + Re_c*sin(NODES);
V1 = Re_d*cos(NODES)  - Im_d*sin(NODES);
V2 = Im_d*cos(NODES)  + Re_d*sin(NODES);
% ----------------------------------------
U0 = zeta1*U1 + zeta2*U2;
a1 = zeta2; a2 = - zeta1;
b1 = sum(diag(V1*(B*U0)'))/n;
b2 = sum(diag(V2*(B*U0)'))/n;
U  = U0; W = zeros(p,n); omga = 0; mu = 0;
% -- iteration of modified Newton method --
for iter = 1:maxit
   % -- compute with trig. Kollokation --------
   % -- RS = - [omga*W' + mu*B*U + f(mu,U)] ---
   RS  = trig_koll(W);
   for i = 1:n
      UP  = U(:,i);
      VP  = mu*B*W(:,i) + feval(F,2,mu,UP,Parmeter);
      RS(:,i) = - (omga*RS(:,i) + VP);
   end;
   % -- solve bifurcation equations -------------
   r1  = sum(diag(V1*RS.'))/n;
   r2  = sum(diag(V2*RS.'))/n;
   det = a1*b2 - a2*b1;
   if det ~= 0
      omganeu = (b2*r1 - b1*r2)/det;
      muneu   = (a1*r2 - a2*r1)/det;
   else
      if zeta1 ~= 0
         omganeu = r1/zeta1;
      else
         omganeu = r2/zeta2;
      end
      muneu = 0;
   end;
   % compute right side of operator equations ---
   % with trig. Kollokation -------------
   % RS = - [omga*W' + mue*B*U + f(mue,U)] ------
   % RS = RS - omganeu*U0' - muneu*B*U0 -----------
   RS = trig_koll(W);
   for i = 1:n
      UP  = U(:,i);
      VP  = mu*B*W(:,i) + feval(F,2,mu,UP,Parmeter);
      RS(:,i) = - (omga*RS(:,i) + VP);
   end;
   [W,PHI]  = trig_koll(U0);
   RS = RS - omganeu*W - muneu*B*V;
   RS = [RS(:);0;0];
   % solve operator equations -------------------
   AA = omga0*kron(PHI,eye(p)) + kron(eye(n),A);
   UU = [U1(:)';U2(:)'];
   AA = [AA;UU];
   W = AA\RS;
   W = reshape(W,p,n);
   Wnorm = norm(W);
   U = U0 + W;
   omga  = omganeu; mu = muneu;
   iter_omga_mu = [iter,omga,mu]
end
% end of iteration ----------------------
omga = omga0 + omga;
% -- compute residuum -------------------
W = trig_koll(U);
for i = 1:n
   UP     = U(:,i);
   VP     = (A + mu*B)*UP + feval(F,2,mu,UP,Parmeter);
   W(:,i) = omga*W(:,i) + VP;
end;
% -- compute relative residuum ----------
period  = 2*pi/omga;
unorm   = sum(diag(U*U'))/n;
unorm   = sqrt(unorm);
wnorm   = sum(diag(W*W'))/n;
wnorm   = sqrt(wnorm);
relres  = wnorm/unorm;
period_relres = [period,relres]
