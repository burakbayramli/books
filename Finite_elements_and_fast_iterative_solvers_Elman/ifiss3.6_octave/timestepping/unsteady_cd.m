%UNSTEADY_CD unsteady CD problem in square domain 
%   IFISS scriptfile: DJS; 27 May 2012. 
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
global pde
pde=12; domain=1;
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
   %eplot(epp,ev,xy,x,y,8); title('SUPG scale factors');
   supg=1; 
   S = femq1_cd_supg(xy,ev,expe,eph,epw); A= A+S; 
end
%
%% set parameters
fprintf('Discrete Convection-Diffusion equation ODE system ...\n')
tfinal = default('target time? (default 100)',100);
tol = default('accuracy tolerance? (default 1e-4)',1e-4);
nstar = default('averaging frequency? (default 10)',10);
dtzero=1e-9; uzero=zeros(size(f));
%% compute solution
tic,
[DT,U,Udot,time] = stabtr(xy,bound,A,Q,f,uzero,dtzero,tfinal,tol,nstar,0);
etoc=toc; fprintf('Integration took  %8.3e seconds\n\n',etoc) 
save square_unsteadycd.mat DT U time viscosity
%
% visualise solution and hold the timestep sequence plot
marker = default('plotting timestep sequence ... \nspecify plot symbol without quotes, default "bo"','bo');
dttplot(DT,99,marker)
pause(3)
square_heatplot(U(:,2:end),time(2:end),xy,x,y,0.1,29)
