function [U,omga,errorcode] = contin(name,Y,Parcont,Parmeter);
% program continues with respect to mue the solution of
% manual continuation
% -- omega*u' + f(mu,u) = 0 in R^p, u(0) = u(2*pi),
% time discretization t = 2*pi/n
% start values omega, mu, U, given by hopf.m
% FUNCTIONS:
% bdf.m,  cg_lq.m, bsp0x.m
% ----------------------------------------------------
F      = [name];
p      = size(Y,1);
n      = size(Y,2);
tol = Parcont(1); maxit  = Parcont(2); omga = Parcont(3);
mu = Parcont(4);
a = 2*pi/n; I = 1:n; NODES  = a*I'; U = Y;
errorcode = 0;
%-- preparation --------------------------------------
mufix = mu; done = 0; antc = 1;
while antc
   [antc,antp,antm,ants,mufix,tol] = driver_c(mufix,tol);
   if antp == 1
      V = [U U(:,1)];
      clf
      plot(V(1,:),V(2,:));
      axis ('square', 'equal');
      pause(1)
   end;
   if ants == 1
      save datenC1 Y omga mufix Parmeter
   end
   %if antm == 1
      %meshhalf
   %end;
   if antc == 0
      return
   end
   % compute residuum --------------------------------
   V  = bdf(1,U);
   VP = feval(F,3,mufix,U,Parmeter);
   W  = omga*V + VP;
   % --Newton's method -------------------------------
   wnorm = sqrt(sum(diag(W*W'))/n);
   it    = 0;
   done  = ((~antc) | (wnorm < tol) | (it > maxit));
   while ~done
      it      = it + 1
      W       = - W;
      [Z,d_omga,errorcode] = cg_lq(F,omga,mufix,tol,U,V,W,Parmeter);
      U       = U + Z;
      omga    = omga + d_omga;
      % compute residuum -----------------------------
      V       = bdf(1,U);
      VP      = feval(F,3,mufix,U,Parmeter);
      W       = omga*V + VP;
      % ---------------------------------------------
      unorm   = sqrt(sum(diag(U*U'))/n);
      wnorm   = sqrt(sum(diag(W*W'))/n);
      d_omga;
      relres  = wnorm/unorm;
      periode = 2*pi/abs(omga);
      d_omga_relres_periode = [d_omga, relres, periode]
      done    = ((~antc) | (wnorm < tol) | (it > maxit));
   end
end
