%SNAPSHOT_SOLVEBOUSS solution of predefined Boussinesq flow problem
%IFISS scriptfile: DJS; 20 July 2013.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.
global L H Pr Ra
if exist('pde','var')==0,
   error('You need to set up a discrete Bousinesq problem first!'), 
end
if exist('soltime','var')==0,
    error('No unsteady Boussinesq solution data in the workspace!'), 
end

fprintf('\nIterative solution of a SNAPSHOT Boussinesq linear system')
fprintf('\nEnclosed flow is NOT assumed ...\n')

if exist('grid','var')==0,
error('The *_grid1h.mat file needs to be loaded in the workspace!'), 
end
if exist('spmat','var')==0,
error('The *_bouss_nobc.mat file needs to be loaded in the workspace!'),
end

fprintf('\nSolution data available for %g seconds',soltime(end))  
fprintf('\n              start time is %g seconds\n',soltime(1)) 
snaptime = default('Approximate time for the SNAPSHOT? (default is the end)',soltime(end));
timek=find(snaptime >= soltime,1,'last');
fprintf('Time step number %g',timek) 
fprintf('\nTime step is %g',solDT(timek))
fprintf('\nConstructing system at time %g seconds',soltime(timek)) 

% set up system at snapshop time
viscosity =sqrt(Pr/Ra);  viscosityT =1/sqrt(Pr*Ra);

%%% unpack grid and matrix data
tout=1; unpack_boussdata
[nv,dd]=size(xyv(:,1)); nuv=2*nv; nnv=nv;
[np,dd]=size(xyp(:,1)); [ntt,dd]=size(xyt(:,1));
boundv=bnd_d; boundt=bnd_dn2; 
gzero=zeros(np,1);
%%
n=timek; t=soltime(n); 
dt=solDT(n); dt0=solDT(n-1);
 u=U(:,n); ub=U(:,n-1);  udot=Udot(:,n); 
tt=T(:,n);               tdot=Tdot(:,n);
ww = (1+(dt/dt0))*u - (dt/dt0)*ub;  
flowsol=[ww;gzero];
   if qmethod==12,  
   Nv = navier_q2(xyv,mv2,ww,0);
   Jnst = 2*Qv + dt*viscosity*Av + dt*[Nv, sparse(nv,nv); sparse(nv,nv), Nv];
   fnst = Qv*udot + M*tt ...
          - (viscosity*Av + [Nv, sparse(nv,nv); sparse(nv,nv), Nv])*u;
   Nt = conv_q2t(xyt,mt2,ww);
   Tnst = Qt + .5*dt*viscosityT*At + .5*dt*Nt;
   hnst = Qt*tdot - (viscosityT*At + Nt)*tt;
   [Jbc,Bbc,Mbc,Tbc,fbc,gbc,hbc] = ...
       dtboussbc(Jnst,B,M,Tnst,fnst,gzero,hnst,xyv,xyt,boundv,boundt,t,t+dt);
   resbc = [fbc;gbc;hbc];
  %% xns = [Jbc,dt*Bbc',-0.5*dt*Mbc;dt*Bbc,-dt*dt*C,sparse(np,ntt);...
  %%        sparse(ntt,nuv),sparse(ntt,np),Tbc]\[fbc;dt*gbc;hbc];
   else
      error('solvers for this discretization are not available!'); 
   end
%%
%% matrix-vector multiply 
   afun_par = struct('Afun','a_bouss','F',Jbc,'B',Bbc,'D',sparse(np,np), ...
                         'Mt',0.5*dt*Mbc,'Ft',Tbc);

%
% select Krylov subspace method and set parameters 
itmeth = 1; fprintf('\n\nGMRES\n');%default('\nGMRES/Bicgstab(2)  1/2 (default GMRES)',1);
   tol = default('stopping tolerance? (default 1e-8)',1e-8);
   maxit = default('maximum number of iterations? (default 100)',100);
   
% select preconditioner
fprintf('preconditioner:\n');
fprintf('   0  none\n');
 %  fprintf('   1  unscaled least-squares commutator (BFBt)\n');
 %  fprintf('   2  pressure convection-diffusion (PCD)\n');
 fprintf('   3  least-squares commutator (LSC)\n');
 fprintf('   5  boundary-adjusted least-squares commutator (LSC*)\n');
 fprintf('   9  modified pressure convection-diffusion (PCD*)\n');
 precon = default('default is PCD*',9);
   if precon>0,
      query = 'ideal / AMG iterated  preconditioning? 1/2 (default ideal)';
      precon_format = default(query,1);
      if (precon_format>2 ),
	     error(' Aborted: preconditiong is not implemented!')
      end
   else
      precon_format = 1;
   end
%
% set structure for preconditioner
   n_null = null_pressure_index(domain,qmethod,size(Bbc,1));
   if precon==2,      %%%% original PCD	   
       [Ap,Fp] = fpsetup_q1(xyv,xyp,mv2,mp1,flowsol,viscosity,domain); 
       Fp = 2*Qp + dt*Fp;   %%includes time-stepping term
   elseif precon==9,  %%%% new PCD   
       if exist('map','var')==0,
          [dddx,dddy,dddxy,dddxyp,dddmp,map] = q2q1gridx(x,y,xy2,mv2,boundv); 
       end
       [Ap,Fp] = fpzsetup_q1(xyv,xyp,mv2,mp1,flowsol,viscosity,domain,map);
       Fp = 2*Qp + dt*Fp;    %%includes time-stepping term
   end
   if precon_format==1, 
   % ideal preconditioning: use direct sparse elimination to solve block systems 
   % in preconditioner
      if precon==0,       %%% no preconditioning
         fprintf('no preconditioning ...\n')
         mfun_par=struct('Mfun','m_nonet');
      elseif precon==1,   %%% old (unscaled) least squares (BFBt) 
         error('No unscaled least-squares commutator preconditioning!!\n')
      elseif precon==2,   %%%%  pressure commutator (oldPCD) 
         error('No ideal pressure convection-diffusion preconditioning!!\n')
      elseif precon==9,   %%%%  modified pressure commutator (newPCD) 
         fprintf('ideal pressure convection-diffusion preconditioning ...\n')
         mfun_par = struct('Mfun','m_bouss_xfp','Fp',Fp,'Ap',Ap,'Mp',Qp,...
                           'domain',domain, 'n_null',n_null,    'G',Qv);
      elseif precon==3,   %%%% precon==3, LSC
         fprintf('ideal least-squares commutator preconditioning ...\n')
         mfun_par = struct('Mfun','m_bouss_xbfbt','domain',domain,'G',Qv,...
                           'n_null',n_null);
      else                %%%% precon==5, boundary adjusted LSC
         bcweights = m_bcscale(xyv,domain,qmethod);
         mfun_par = struct('Mfun','m_bouss_xbfbt_bc','domain',domain,'G',Qv,...
                           'n_null',n_null,'weights',bcweights); 
      end 
   else 
   % precon_format==2, iterated preconditioning with algebraic multigrid 
   % setup velocity convection-diffusion structures
      global  amg_gridA amg_smootherA amg_gridF amg_smootherF
   %  compute new MG data or reload existing data?
      compute_mg = default('compute / load convection-diffusion AMG data? 1/2 (default 1)',1);
      if compute_mg==2
         gohome, cd datafiles, load amgdata_cd.mat
      else
         amg_gridF = amg_grids_setup(Jbc);
         fprintf('\n')
         gohome, cd datafiles, save amgdata_cd.mat amg_gridF
      end
      damps = default('AMG fine level smoothing strategy? PDJ/ILU 1/2  (default ILU)',2);
      if damps==1
         fprintf('point damped Jacobi smoothing on finest level ..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'PDJ', 2);
      elseif damps==0
         fprintf('point GS smoothing on finest level..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'PGS');
      else 
         fprintf('ILU smoothing on finest level..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'ILU',1);
      end
      amg_smootherF = amg_smoother_setup(amg_gridF, smoother_paramsF);   
	  if precon==1,         %%% AMG iterated BFBt
         fprintf('AMG iterated BFBt preconditioning ...\n')             
         error('try AMG with scaled least-squares commutator preconditioning instead!')
      elseif precon==2      %%% AMG iterated PCD
         error('Not implemented yet!')  
      elseif precon==3, %%% AMG iterated LSC 
         fprintf('AMG iterated LSC preconditioning ...\n')
         Gdiag=spdiags(diag(Qv),0,nuv,nuv); 
         n_null=0; %%force grid setup for singular matrix
         if n_null>0,  Apx = pressurebc(Bbc*(Gdiag\Bbc'),n_null); 
                       amg_gridA = amg_grids_setup(Apx);
         else amg_gridA = amg_grids_setup(Bbc*(Gdiag\Bbc')); 
         end
         fprintf('BinvGB setup done.\n')
         if damps==1
           fprintf('point damped Jacobi smoothing on finest level ..\n')
           smoother_paramsA = amg_smoother_params(amg_gridA, 'PDJ', 2);
         elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PGS');
         else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'ILU',1);
         end
         amg_smootherA = amg_smoother_setup(amg_gridA, smoother_paramsA);       
         global  amg_gridFt amg_smootherFt   
         amg_gridFt = amg_grids_setup(Tbc);
         fprintf('Temperature AMG grid setup done.\n')
         plot_mg = 2; %default('plot AMG grid sequence? yes/no 1/2 (default no)',2);
         if plot_mg==1, amg_coarsen_plot(amg_gridFt, xyt); end
         if damps==1
            fprintf('point damped Jacobi smoothing on finest level ..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'PDJ', 2);
         elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'PGS');
         else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'ILU',1);
         end
         amg_smootherFt = amg_smoother_setup(amg_gridFt, smoother_paramsFt);
         mfun_par = struct('Mfun','m_bouss_xbfbt_amgz','domain',domain,...
                           'Gdiag',Gdiag,'n_null',n_null);
      elseif precon==9,      %%% AMG iterated PCD*
         fprintf('AMG iterated PCD* preconditioning ...\n')         
         Gdiag=spdiags(diag(Qv),0,nuv,nuv);
         amg_gridA = amg_grids_setup(Bbc*(Gdiag\Bbc'));
         fprintf('BinvGB AMG setup done.\n')
         if damps==1
            fprintf('point damped Jacobi smoothing on finest level ..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PDJ', 2);
         elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PGS');
         else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'ILU',1);
         end
         amg_smootherA = amg_smoother_setup(amg_gridA, smoother_paramsA);               
         global  amg_gridFt amg_smootherFt   
         amg_gridFt = amg_grids_setup(Tbc);
         fprintf('\nTemperature AMG grid setup done.\n')
         plot_mg = 2; %default('plot AMG grid sequence? yes/no 1/2 (default no)',2);
         if plot_mg==1, amg_coarsen_plot(amg_gridFt, xyt); end
         if damps==1
            fprintf('point damped Jacobi smoothing on finest level ..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'PDJ', 2);
         elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'PGS');
         else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsFt = amg_smoother_params(amg_gridFt, 'ILU',1);
         end
         amg_smootherFt = amg_smoother_setup(amg_gridFt, smoother_paramsFt);
         mfun_par = struct('Mfun','m_bouss_xfp_amgz','Fp',Fp,'G',Qv,'Mp', ...
                            Qp,'domain',domain,'n_null',n_null); 
      else             %%% Place for precon=5
         error('Boundary-adjusted LSC with AMG not implemented yet');
      end
   end
%
% solve using GMRES or BiCGSTAB(2)
% zero initial guess
   x0=zeros(size(resbc)); rhs=resbc; 
   tic %%start timing
   if itmeth==1, %GMRES 
      fprintf('GMRES iteration ...\n');
      params = [tol,maxit,1];
      [x_it,flag,iter,resvec] = gmres_r(afun_par,mfun_par,rhs,params,x0);
   else %BiCGSTAB(2)
      fprintf('BiCGSTAB(2) iteration ...');
      params = [tol,maxit,2,.7];  
      [x_it,flag,iter,resvec] = bicgstab_ell_r(afun_par,mfun_par,rhs,params,x0);
   end
   etoc = toc;
%end
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Print and plot results
if flag ==0,
   % successful convergence
   fprintf('convergence in %3i iterations\n',iter)
   nr0=resvec(1);
   fprintf('\n    k  log10(||r_k||/||r_0||)   \n')
   for its=1:iter+1,
      fprintf('%5i %16.4f \n', its-1, log10(resvec(its)/nr0));
   end
   fprintf('Bingo!\n')
   fprintf('\n  %9.4e seconds\n\n\n',etoc)  
   %%% plot relative residual
   resplot(resvec/nr0)
   %%plot solution
   upit  = dt*x_it(1:2*nnv) + u;  tpit  = tt + .5*dt*x_it(2*nnv+np+1:end);
  % bouss_cavityplot(upit,tpit,time(timek),xyv,xyt,grid(1).x,grid(1).y,L,H,hty,.1,100)
else
   nr0=resvec(1);
   fprintf('\n    k  log10(||r_k||/||r_0||)   \n')
   for its=1:iter+1,
      fprintf('%5i %16.4f \n', its-1, log10(resvec(its)/nr0));
   end
   fprintf('iteration aborted! Iteration returned with flag equal to  %2i \n',flag)
   %%% plot residuals
   resplot(resvec)
end