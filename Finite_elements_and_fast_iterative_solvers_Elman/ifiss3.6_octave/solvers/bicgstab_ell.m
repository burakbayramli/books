function [x, flag, k, errv] = bicgstab_ell(aparams,mparams, b, params, x0) 
%BICGSTAB_ELL bicgstab(ell) for right-preconditioned iterates
%   [x, flag,iter,resvec] = bicgstab_ell(aparams,mparams, b, params, x0)
%   input
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%          b            right-hand side vector
%          params       four-dimensional vector to control iteration
%            params(1) = relative residual reduction factor
%            params(2) = max number of iterations 
%            params(3), params(4):  see source file for definitions
%          x0           initial preconditioned iterate
%   output
%          x            computed (approximate) preconditioned solution
%          flag         convergence flag
%            0 for convergence to params(1) within params(2) iterations
%            1 if iterated params(2) times but did not converge to params(1)
%          iter         total iteration count
%          resvec       vector of residual norms of iterates
%
%   called by bicgstab_ell_r
%   IFISS function: HCE; 15 March 2005.

%Modification of BiCGstab(ell) written by Gerard Sleijpen.
%Copyright 1997 Gerard Sleijpen.  
%Reproduced and distributed with permission of the copyright holder.
%Modifications to handle IFISS data structures identified in line.


%function [x, flag, k, errv] = bicgstab_ell(aparams,mparams, b, params, x0)

% *************************************************************************
% Modification of BiCGstab(ell) routine as documented below, to handle
% IFISS matrix operations:  calls to "operate" replaced by calls to IFISS 
% mvp and preconditioning routines.  
% Note: this program is designed for a right-preconditioned system with 
% inital guess x0 for the preconditioned iterate.  Adapted via the wrapper
% "rbcg_ell.m" to handle an initial guess for the unpreconditioned iterate 
% by using the residual as the right hand side and solving for the
% correction starting with zero initial guess.
%
% Modifications by HCE, 14 January 2005.
% *************************************************************************
%
% BiCGstab(ell) algorithm by Sleijpen & Fokkema 1993
% enhanced with modifications by Sleijpen & van der Vorst
%
% Version 22.01.97 by Gerard Sleijpen
%
% Computes the solution of Ax=b using the BiCGstab(ell) scheme.
% If a preconditioning matrix M is provided,
% bicgstabl will implicitly solve the system A*C y=b, where C=M^(-1),
% and return the solution x=C*y.
%
% Startvector:                      x0
% parameters:                       params=[tol,maxiter,ell,kappa]
%     having the following meaning.
% Maximal number of iterations:     maxiter
% Desired residual reduction:       tol
% Value of ell:                     ell
%    - BiCGstab(ell) is a "product" of BiCG and GMRES(ell)            -
%    - ell=1 (& kappa=0) then BiCGstab(1) = BiCGSTAB                  -
%    - specifically for skew problems                                 -
%    -                      ell=2 (default) or 4 is better choice     -
% Maximum value |cos(r,Ar)|:        kappa
%    - An average of GMRES(ell) and FOM(ell)                          -
%    -                      if angle(r,Ar) is large (> 45 degr),      -
%    - can improve the stability of the BiCG part:                    -
%    - kappa:=max|cos(r,Ar)| controles                                -
%    -       the switch from GMRES(ell) to average.                   -
%    - kappa=0.7 corresponds to angle of 45 degr (default choice).    -
%    - kappa=0 then in all steps GMRES(ell) (standaard BiCGstab(ell)) -

% In operate.m action of preconditioned matrix A*C

% Each iteration step of BiCGSTAB requires 2 MVs (precond. matrix actions)
% Each it. step, BiCGstab(ell) requires 2*ell MVs.
% Computational costs average per MV are more or less independent of ell
% Therefore, for fair comparision, in this code: 1 iteration = 2 MV.


% Names of mvp and preconditioning routines
afun = aparams.Afun;
mfun = mparams.Mfun;

%unprecond=0;

tol = params(1);
maxiter = params(2);
l = params(3);
kappa=params(4);

% Note:  initial iterate here is for preconditioned quantity
x = x0;

n = size(b,1);
%y = x; operate; r = b - y;
%% Modification by HCE, adapt for IFISS data structures
r = b - feval(afun, feval(mfun,x,aparams,mparams), aparams);
rt = r;
%% random shadow vector rt may work better for skew problems
rand('seed',0), rt = rand(n,1);
u = zeros(n,1);
gamma=1; omega=1; k = 0;


norm_r=norm(r); r0_norm = norm_r; errv(1,1)=1;

L=2:l+1; ZERO=zeros(l,1);

%PRINTS0='  %5i     %8.4f ';PRINTS=[PRINTS0,' bicg\n'];

while ( k<maxiter & errv(k+1,1)>tol )
   gamma=-omega*gamma; y=r;
   for j=1:l
       rho=rt'*y;   beta=rho/gamma;
       u=r-beta*u;
%      y=u(:,j); operate; u(:,j+1)=y;
%% Modification by HCE, adapt for IFISS data structures
       y = feval(afun, feval(mfun,u(:,j),aparams,mparams), aparams); 
       u(:,j+1) = y;
       gamma=rt'*y; alpha=rho/gamma;
       x=x+alpha*u(:,1);
       r=r-alpha*u(:,2:j+1);
%      y=r(:,j); operate; r(:,j+1)=y;
%% Modification by HCE, adapt for IFISS data structures
       y = feval(afun, feval(mfun,r(:,j),aparams,mparams), aparams);
       r(:,j+1) = y;
   end
   G=r'*r;
   % norm_r=sqrt(G(1,1)); errv(k+L,1)=ZERO+norm_r/r0_norm; k=k+l;
   %%  fprintf(PRINTS,k,log10(errv(k+1,1))); PRINTS=[PRINTS0,' mr\n'];

   Gamma0=[1;-G(2:l,2:l)\G(2:l,1);0];
   Gamma1=[0;-G(2:l,2:l)\G(2:l,l+1);1];
   NGamma0=Gamma0'*G*Gamma0; NGamma1=Gamma1'*G*Gamma1;
   omega=Gamma0'*G*Gamma1;
   cosine=abs(omega)/sqrt(NGamma0*NGamma1); omega=omega/NGamma1;
   if cosine<kappa, omega=(kappa/cosine)*omega; end %PRINTS=[PRINTS0,' +\n']; end
   Gamma=Gamma0-omega*Gamma1;

   x=x-r*[Gamma(2:l+1);0];
   r=r*Gamma; u=u*Gamma;

   norm_r=norm(r); errv(k+L,1)=ZERO+norm_r/r0_norm; k=k+l;
   %%  fprintf(PRINTS,k,log10(errv(k+1,1))); PRINTS=[PRINTS0,' bicg\n'];
end   % while

% Unprecondition the solution
%unprecond=1; operate;
%x = feval(mfun,x,aparams,mparams);
if errv(k+1,1)>tol, flag = 1; else flag = 0; end
errv = errv*r0_norm;
