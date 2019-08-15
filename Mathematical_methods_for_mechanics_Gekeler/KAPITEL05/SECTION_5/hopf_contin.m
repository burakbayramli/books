function [U,omga,errorcode] = hopf_contin(name,Y,Parcont,Parmeter);
% program continues with respect to mue the solution of
% -- omega*u' + f(mue,u) = 0 in R^p, u(0) = u(2*pi),
% time discretization t = 2*pi/n
% start values given by HOPF_BDF.M: omega, mue, U
% FUNCTIONS: bdf.m, bdft.m, cg_lq.m
% ----------------------------------------------------
errorcode = 0; F = [name]; p = size(Y,1); n = size(Y,2);
tol  = Parcont(1); maxit = Parcont(2);
omga = Parcont(3); mue   = Parcont(4);
a = 2*pi/n; I = 1:n; NODES  = a*I.';
U = Y; 
%-- preparation --------------------------------------
% compute residuum --------------------------------
BDFU  = back_diff(1,U);
W = omga*BDFU + feval(F,3,mue,U,Parmeter);
% --Newton's method -------------------------------
wnorm = sqrt(sum(diag(W*W.'))/n);
iter = 0; done = wnorm < tol;
while ~done
   iter = iter + 1; W = - W;
   [Z,d_omga,errorcode] = cg_lq(F,omga,mue,tol,U,BDFU,W,Parmeter);
   U = U + Z; omga = omga + d_omga;
   % compute residuum -----------------------------
   BDFU = back_diff(1,U);
   W    = omga*BDFU + feval(F,3,mue,U,Parmeter);
   % ---------------------------------------------
   unorm = sqrt(sum(diag(U*U.'))/n);
   wnorm = sqrt(sum(diag(W*W.'))/n);
   znorm = norm(Z);
   relres  = wnorm/unorm;
   periode = 2*pi/abs(omga);
   it_d_omga_relres_periode = [iter,relres, periode]
   done    = (wnorm < tol) | (iter > maxit);
end
if iter > maxit, errorcode = 2; end
