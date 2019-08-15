function [U,omga,mue,errorcode] = hopf_trig1(name,Parhopf,Parmeter);
% Hopf bifurcation with trig. collocation
% direct iteration

errorcode = 0; F = [name]; n = Parhopf(1); tol = Parhopf(2);
maxit = Parhopf(3); Eps = Parhopf(4);
zeta1 = Parhopf(5); zeta2 = sqrt(1 - zeta1^2);
zeta1 = Eps*zeta1;  zeta2 = Eps*zeta2;
a = 2*pi/n; I = 1:n; NODES = a*I;
mue = []; X = [];
Y = feval(F,1,mue,X,Parmeter); p = size(Y,1);
A     = Y(:,1:p); B = Y(:,p+1:2*p);
Re_c  = Y(:,2*p+1); Im_c = Y(:,2*p+2);
Re_d  = Y(:,2*p+3); Im_d = Y(:,2*p+4);
omga0 = Y(1,2*p+5);
% -- discretization of exact eigensolutions --
U1 = Re_c*cos(NODES)  - Im_c*sin(NODES);
U2 = Im_c*cos(NODES)  + Re_c*sin(NODES);
V1 = Re_d*cos(NODES)  - Im_d*sin(NODES);
V2 = Im_d*cos(NODES)  + Re_d*sin(NODES);
% --------------------------------------------
U0 = zeta1*U1 + zeta2*U2;
a1 = zeta2; a2 = - zeta1;
U = U0; W = zeros(p,n); omga = 0; mue = 0;
% -- direct iteration --------
for iter = 1:maxit
   % -- Solve bifurcation equation --------
   b1 = sum(diag(V1*(B*U).'))/n; b2 = sum(diag(V2*(B*U).'))/n;
   % -- Matrix for bif. equation : [a1, b1; a2, b2] -----
   RS = - feval(F,2,mue,U,Parmeter);
   r1  = sum(diag(V1*RS.'))/n; r2  = sum(diag(V2*RS.'))/n;
   det = a1*b2 - a2*b1;
   if det ~= 0
      omga = (b2*r1 - b1*r2)/det;
      mue   = (a1*r2 - a2*r1)/det;
   else
      if zeta1 ~= 0, omga = r1/zeta1;
      else,          omga = r2/zeta2;
      end
      mue = 0;
   end;
   % -- Solve operator equation ------------------
   [TRIGU,PHI] = trig_koll(U);
   RS  = - (omga*TRIGU + mue*B*U + feval(F,2,mue,U,Parmeter));
   RS = [RS(:);0;0];
   AA = omga0*kron(PHI,eye(p)) + kron(eye(n),A);
   VV = [V1(:).';V2(:).'];
   AA = [AA;VV]; W = AA\RS; W = reshape(W,p,n);
   U = U0 + W;
   iter_omga_mue = [iter,omga0 + omga,mue]
end
% end of iteration ----------------------
omga = omga0 + omga;
% -- compute relative residuum -------------------
TRGU = trig_koll(U);
RES = omga*TRGU + feval(F,3,mue,U,Parmeter);
period  = 2*pi/omga;
unorm   = sqrt(sum(diag(U*U'))/n);
wnorm   = sqrt(sum(diag(W*W'))/n);
relres  = wnorm/unorm;
period_relres_det = [period,relres,det]
