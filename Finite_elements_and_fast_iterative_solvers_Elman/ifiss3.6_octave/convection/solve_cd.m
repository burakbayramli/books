%SOLVE_CD solve convection-diffusion problem in square domain
%   IFISS scriptfile: DJS; 27 May 2012. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
pde=2; domain=1;
global viscosity
%% load assembled matrices
gohome
cd datafiles
load square_cd_nobc.mat
%
viscosity=default('viscosity parameter (default 1/200)',1/200);
%% update cofficient matrix
supg=0;
A = viscosity*A + N; f = zeros(size(xy(:,1)));
%% compute element peclet numbers
epe = epe/viscosity;
eplot(epw,ev,xy,x,y,20,'element wind');
fprintf('maximum element Peclet number is %10.6e\n',max(epe))
%
%% include streamline diffusion matrix (if necessary)
esupg=find(epe<=1); expe=epe;   %expe(esupg)=0;
if any(expe), 
   supg=default('SUPG parameter (default is optimal)',inf);
   if isinf(supg)
      expe=0.5*(1-1./expe);
      expe(esupg)=inf;
   else
      expe=ones(size(expe)); expe=supg*expe; expe(esupg)=inf;
   end
   epp=expe; epp(esupg)=0; epp=epp.*eph./epw;
   %eplot(epp,ev,xy,x,y,8,'SUPG scale factors');
   supg=1; 
   S = femq1_cd_supg(xy,ev,expe,eph,epw); A= A+S; 
end
%
%% boundary conditions
[Asupg,fsupg] = nonzerobc(A,f,xy,bound);
%
%% save resulting system
fprintf('system saved in square_cd.mat ...\n')
gohome
cd datafiles
save square_cd.mat qmethod supg viscosity Asupg Q fsupg xy x y 
%% compute solution
tic
fprintf('solving linear system ...  ')
xsupg=Asupg\fsupg;
fprintf('done\n')
etoc=toc; fprintf('linear system solved in  %8.3e seconds\n\n',etoc) 
save square_cd.mat xsupg  -append 
%
%% compute a posteriori error estimate
   [jmp,els] = oldq1fluxjmps(xsupg,xy,ev,ebound);
   [elerror,fez,aez] = cdpost_p(viscosity,xsupg,jmp,els,xy,ev,ebound);
   [error_p,elerror_p] = cdpost_bc(viscosity,aez,fez,elerror,xy,ev,ebound);
   save square_cd.mat elerror_p ev -append 
   errplot(xsupg,elerror_p,ev,xy,x,y,22)
