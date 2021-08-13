%SNAPSHOT_SOLVEFLOW solution of predefined unsteady problem
%IFISS scriptfile: DJS; 13 October 2013.
% Copyright (c) 2009 D.J. Silvester

% Declare global variables for scalar and vector problems
global amg_grid amg_smoother   
global amg_gridA amg_smootherA amg_gridF amg_smootherF amg_gridAw amg_smootherAw

if exist('pde','var')==0,
   error('You need to set up a specific discrete problem first!'), 
end
if exist('time','var')==0,
    error('No unsteady PDE solution data in the workspace!'), 
end
fprintf('\nIterative solution of a SNAPSHOT linear system')
fprintf('\nSolution data available for %g seconds\n',time(end))  
snaptime = default('Approximate time for the SNAPSHOT? (default is the end)',time(end));
timek=find(snaptime >= time,1,'last');
fprintf('\nTime step number %g',timek) 
fprintf('\nConstructing system at time %g seconds',time(timek)) 
fprintf('\n    current timestep is %g seconds\n',DT(timek)) 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if pde==11,
   %%% DIFFUSION Problem
   fprintf('\ndiscrete diffusion ODE system ...\n')
   ttime=time(timek); dt = DT(timek);  
   u= U(:,timek); udot = Udot(:,timek);
   [Hgal,fgal] = dtheatbc((M+(.5*dt)*A),f + M*udot - A*u,xy,bound,ttime,ttime+dt); 
   % select Krylov subspace method
   itmeth = default('PCG/MINRES? 1/2 (default PCG)',1);

   % set parameters
   tol = default('tolerance? (default 1e-8)',1e-8);
   maxit = default('maximum number of iterations? (default 100)',100);
   
   % select preconditioner and construct it
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   2  incomplete cholesky\n');
   fprintf('   4  algebraic multigrid\n');
   precon = default('default is AMG ',4);
   if precon==0,     % none
      M1=[]; M2=[]
   elseif precon==2, % incomplete Cholesky
    M1 = ichol(Agal); M2=M1';
 %   M2 = cholinc(Hgal,'0'); M1=M2'; fprintf('--- cholinc.m\n')%
   elseif precon==4, % AMG
   % uses global variables amg_grid amg_smoother
   amg_grid = amg_grids_setup(Hgal);
   fprintf('setup done.\n')
   plot_mg = default('plot AMG grid sequence? yes/no 1/2 (default no)',2);
   if plot_mg==1, amg_coarsen_plot(amg_grid, xy); end
   smoothopt = default('PDJ/PGS smoother? 1/2 (point damped Jacobi)',1);
      if smoothopt==1
      fprintf('point damped Jacobi smoothing ..\n')
      smoother_params = amg_smoother_params(amg_grid, 'PDJ', 2);
      else
      fprintf('point Gauss-Seidel smoothing ..\n')
      smoother_params = amg_smoother_params(amg_grid, 'PGS', 2);
      end
   amg_smoother = amg_smoother_setup(amg_grid, smoother_params); 
   else
      error('invalid preconditioner!')
   end
%
   % zero initial guess
   x0=zeros(size(fgal));
   tic %%start timing
   if itmeth==1, %PCG
      fprintf('\nPCG iteration ...\n');
      if precon<=2, 
         [x_it,flag,relres,iter,resvec] = pcg(Hgal,fgal,tol,maxit,M1,M2,x0);
       elseif  precon==4
           [x_amg,flag,relres,iter,resvec] = ...
             pcg(Hgal,fgal,tol,maxit, @amg_v_cycle, [], x0, amg_grid, amg_smoother); 
      end
   elseif itmeth==2, %MINRES
      fprintf('\nMINRES iteration ...\n');
      if precon<=2, 
         [x_it,flag,relres,iter,resvec] = minres(Hgal,fgal,tol,maxit,M1,M2,x0);
      elseif  precon==4
           [x_it,flag,relres,iter,resvec] = ...
              minres(Hgal,fgal,tol,maxit,@amg_v_cycle, [], x0, amg_grid, amg_smoother);
      end
   else
      error('invalid iterative method!')
   end
   etoc = toc;
%
elseif pde==12,
   %%% CONVECTION-DIFFUSION Problem
   fprintf('\ndiscrete convection-diffusion ODE system ...\n')
   ttime=time(timek); dt = DT(timek);  
   u= U(:,timek); udot = Udot(:,timek);
   [Hgal,fgal] = dtheatbc((Q+(.5*dt)*A),f + Q*udot - A*u,xy,bound,ttime,ttime+dt); 
% 
   % set structure for matrix-vector product   
   afun_par = struct('Afun','a_cdt','A',Hgal);
%
   % select Krylov subspace method and set parameters 
   itmeth = 1; %%% GMRES
   tol = default('stopping tolerance? (default 1e-8)',1e-8);
   maxit = default('maximum number of iterations? (default 100)',100);
% 
   % select preconditioner and construct it
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   2  incomplete LU\n');
   fprintf('   4  algebraic multigrid\n');
   precon = default('default is AMG ',4);
   if precon==0,      % none
      mfun_par=struct('Mfun','m_nonet');
   elseif precon==2,  % incomplete LU
      setup.type='nofill';
      [L,U]=ilu(Hgal,setup); 
      mfun_par=struct('Mfun','m_ilut','L',L,'U',U);
   elseif precon==4,  % AMG
  %  uses global variables amg_grid amg_smoother
  %% set up structure for matrix-vector multiply   
   afun_par = struct('Afun','a_cdt','A',Hgal);
  %% 
  % compute new MG data or reload existing data?
   compute_mg = default('compute / load AMG data? 1/2 (default 1)',1);
      if compute_mg==2
         load amgdata_cd.mat
      else
         amg_grid = amg_grids_setup(Hgal);
         fprintf('setup done.\n')
         gohome, cd datafiles, save amgdata_cd.mat amg_grid
         plot_mg = default('plot AMG grid sequence? yes/no 1/2 (default no)',2);
         if plot_mg==1, amg_coarsen_plot(amg_grid, xy); end
      end
       smoothopt = default('PDJ/PGS/LGS/ILU smoother? 1/2/3/4 (point damped Jacobi)',1);
      if smoothopt==1
         fprintf('point damped Jacobi smoothing ..\n')
         smoother_params = amg_smoother_params(amg_grid, 'PDJ', 2);
      elseif smoothopt==2
         fprintf('point Gauss-Seidel smoothing ..\n')
         smoother_params = amg_smoother_params(amg_grid, 'PGS', 2);
      elseif smoothopt==3
         fprintf('alternating line GS smoothing on finest level/ point Jacobi otherwise ..\n')
         smoother_params = amg_smoother_params(amg_grid, 'LGS/PDJ');
      else  
          fprintf('ILU smoothing on finest level..\n')
          smoother_params = amg_smoother_params(amg_grid, 'ILU');
      end
      amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
      mfun_par=struct('Mfun','m_amgzt');
   else
	  error('invalid preconditioner!')
   end
%
% solve using GMRES 
% zero initial guess
   x0=zeros(size(fgal));
   tic %%start timing
   if itmeth==1, %GMRES
      fprintf('\nGMRES iteration ...\n');
      params = [tol,maxit,1];
      [x_it,flag,iter,resvec] = gmres_r(afun_par,mfun_par,fgal,params,x0);
   else
      error('invalid iterative method!')
   end
   etoc = toc;
elseif pde==14,
    %%% NAVIER-STOKES PROBLEM
     if     domain==1,  
      fprintf('enclosed flow (cavity) problem ...\n')
   elseif domain==3,  
      fprintf('inflow/outflow (step) problem ...\n')
   elseif domain==4,  
      fprintf('inflow/outflow (obstacle) problem ...\n')
 %%   error('solvers for this problem/domain are not available!');
   else 
      error('solvers for this problem/domain are not available!');
     end
   % set up system at snapshop time
   n=timek;  t = time(n); [np,nuv]=size(B); nv=nuv/2;
   dt = DT(n);    u = U(:,n);  udot = Udot(:,n);
   dt0 = DT(n-1); ub = U(:,n-1); 
   if qmethod>1,        
      ww = (1+(dt/dt0))*u - (dt/dt0)*ub; N = navier_q2(xy,mv,ww,0);
      flowsol=[ww;gzero];
      Anst = 2*G + dt*viscosity*A + dt*[N, sparse(nv,nv); sparse(nv,nv), N];
      fnst = G*udot -(viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N])*u;
      [Anst,Bst,fzz,gzz] = dtflowbc(Anst,B,fnst,gzero,xy,bound,t,t+dt);
  %%%    xns = [Anst,Bst';Bst,sparse(np,np)]\[fzz;gzz]; 
   else
      error('solvers for this discretization are not available!'); 
      N = navier_q1(xy,ev,ww,0);
   end 
   % set structure for matrix-vector product  
   afun_par = struct('Afun','a_nst','F',Anst,'B',Bst,'D',sparse(np,np));
   %
   % select Krylov subspace method and set parameters 
   itmeth = 1; %% force GMRES 
   tol = default('stopping tolerance? (default 1e-8)',1e-8);
   maxit = default('maximum number of iterations? (default 100)',100); 
   % select preconditioner
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
 %  fprintf('   1  unscaled least-squares commutator (BFBt)\n');
   fprintf('   2  modified pressure convection-diffusion (Fp)\n');
   fprintf('   3  least-squares commutator\n'); 
   fprintf('   4  corrected pressure convection-diffusion (Fp*)\n');
   fprintf('   5  boundary-adjusted least-squares commutator (LSC*)\n');
   precon = default('default is Fp*',4);
   if precon>0,
      query = 'ideal / AMG iterated  preconditioning? 1/2 (default ideal)';
      precon_format = default(query,1);
      if (precon_format>2 ),
	     error(' GMG not implemented as yet!')
      end
   else
      precon_format = 1;
   end
%
   % set structure for preconditioner
   n_null = null_pressure_index(domain,qmethod,size(Bst,1));
   if precon==2,      %%%%  Fp: need construction of Ap and Fp blocks
      if qmethod==2
	     %% set up first edition Fp* operator for Q1 approximation	
         [Ap,Fp] = fpxsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain,map);
 %        [Ap,Fp] = fpsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain); 
         Fp=dt*Fp+2*Q;      %%includes time-stepping term 
         elseif qmethod==3
         %% set up Fp operator for P1 approximation	   
         [Ap,Fp] = fpsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain);
         Fp=dt*Fp+2*Q;      %%includes time-stepping term
      else
          error('FP not yet implemented for this pressure approximation');
      end
   elseif precon==4,      %%%%  corrected Fp: need construction of Ap and Fp blocks 
      if qmethod==1,
        %% set up Fp* operator for Q0 approximation	 
		 [Ap,Fp] = fpzsetup_q0(xy,xyp,ev,ee,flowsol,viscosity,domain);		 
         Fp=dt*Fp+2*Q;      %%includes time-stepping term
      elseif qmethod==2
         %% set up Fp* operator for Q1 approximation	   
         [Ap,Fp] = fpzsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain,map);
         Fp=dt*Fp+2*Q;      %%includes time-stepping term
      elseif qmethod==3
	     %% set up Fp* operator for P1 approximation	   
         [Ap,Fp] = fpzsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain);
         Fp=dt*Fp+2*Q;      %%includes time-stepping term
      elseif qmethod==0
         %% set up Fp* operator for Q1 approximation
	     [Ap,Fp] = fpzsetup_q1(xy,xyp,ev,ev,flowsol,viscosity,domain,(1:np)'); 
         Fp=dt*Fp+2*Q;      %%includes time-stepping term
      else
          error('FP* not yet implemented for this pressure approximation');          
      end
   end
   if precon_format==1, 
   % ideal preconditioning: use direct sparse elimination to solve block systems 
   % in preconditioner
      if precon==0,       %%% no preconditioning
         fprintf('no preconditioning ...\n')
         mfun_par=struct('Mfun','m_nonet');
      elseif precon==1,   %%% old (unscaled) least squares (BFBt) 
      error('No unscaled least-squares commutator preconditioning!!\n')
      elseif precon==2, %%%%  modified pressure commutator (Fp) 
         fprintf('modified pressure convection-diffusion preconditioning ...\n')
         mfun_par = struct('Mfun','m_tfp','Fp',Fp,'G',G,'Mp',Q,'domain',domain,...
                           'n_null',n_null);
      elseif precon==3,   %%%% new (scaled) least squares (BFBt)
         fprintf('ideal least-squares commutator preconditioning ...\n')
         if qmethod>1
         mfun_par = struct('Mfun','m_xbfbt','domain',domain,'G',G,'n_null',n_null);
         elseif qmethod==0,
         [Cp1,Cp2] = Cpre_q1q1(xy,ev);
         mfun_par = struct('Mfun','m_sxbfbt','domain',domain,'G',G,'Q',Q,...
                              'viscosity',viscosity,'Cp1',Cp1,'Cp2',Cp2,...
                               'n_null',n_null); 
         end
      elseif precon==4 ,  %%%%  corrected pressure commutator (Fp) 
         fprintf('corrected pressure convection-diffusion preconditioning ...\n');
         mfun_par = struct('Mfun','m_xfp','Fp',Fp,'Ap',Ap,'Mp',Q,'domain',domain,...
                            'n_null',n_null);
      else     %%%% precon==5, boundary-adjusted least squares 
         fprintf('ideal boundary-adjusted least-squares commutator preconditioning ...\n')
         bcweights = m_bcscale(xy,domain,qmethod);
         if qmethod>1
            mfun_par = struct('Mfun','m_xbfbt_bc','domain',domain,'G',G,'n_null',n_null,...
                              'weights',bcweights); 
         elseif qmethod==0,
			[Cp1,Cp2] = Cpre_q1q1(xy,ev);
			mfun_par = struct('Mfun','m_sxbfbt_bc','domain',domain,'G',G,'Q',Q,...
							  'viscosity',viscosity,'Cp1',Cp1,'Cp2',Cp2,'n_null',n_null,...
                              'weights',bcweights);   
		 elseif qmethod==1,
			[Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain);
			Cp1=beta*Cp1; Cp2=beta*Cp2;
            mfun_par = struct('Mfun','m_sxbfbt_bc','domain',domain,'G',G,'Q',Q,...
                              'viscosity',viscosity,'Cp1',Cp1,'Cp2',Cp2,'n_null',n_null,...
                              'weights',bcweights);
         end    
      end  
   else 
   % precon_format==2, iterated preconditioning with algebraic multigrid 
   % uses global variables amg_gridA amg_smootherA amg_gridF amg_smootherF
   % setup velocity convection-diffusion structures
    amg_gridF = amg_grids_setup(Anst);
      damps = default('AMG fine level smoothing strategy? PDJ/ILU 1/2  (default ILU)',2);
         if damps==1
         fprintf('point damped Jacobi smoothing on finest level ..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'PDJ', 2);
         elseif damps==0
         fprintf('point GS smoothing on finest level..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'PGS',2);
         else 
         fprintf('ILU smoothing on finest level..\n')
         smoother_paramsF = amg_smoother_params(amg_gridF, 'ILU',1);
         end
       amg_smootherF = amg_smoother_setup(amg_gridF, smoother_paramsF);   
	     if precon==1,         %%% AMG iterated BFBt
         fprintf('AMG iterated BFBt preconditioning ...\n')             
         error('try AMG with scaled least-squares commutator preconditioning instead!')
         elseif precon==2      %%% AMG iterated Fp
            fprintf('AMG iterated PCD preconditioning ...\n')         
         Gdiag=spdiags(diag(G),0,nuv,nuv);
         amg_gridA = amg_grids_setup(Bst*(Gdiag\Bst'));
         fprintf('BinvGB AMG setup done.\n')
          if damps==1
	        fprintf('point damped Jacobi smoothing on finest level ..\n')
			smoother_paramsA = amg_smoother_params(amg_gridA, 'PDJ', 2);
			elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PGS',2);
            else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'ILU',1);
          end
          amg_smootherA = amg_smoother_setup(amg_gridA, smoother_paramsA);
          fprintf('Chebyshev iteration preconditioning ..\n')
          number_its = default('number of iterations? (default 5)',5);
          mfun_par = struct('Mfun','m_tfp_amgcheb','Fp',Fp,'G',G,'Mp',Q, ...
            'its',number_its,'qmethod',-qmethod,'domain',domain,'n_null',n_null);  
         elseif precon==3, %%% AMG iterated scaled BFBt 
         fprintf('AMG iterated LSC preconditioning ...\n')
         if qmethod>1,
         Gdiag=spdiags(diag(G),0,nuv,nuv); 
         %n_null=0; %%force grid setup for singular matrix
         if n_null>0,  Apx = pressurebc(Bst*(Gdiag\Bst'),n_null); 
                       amg_gridA = amg_grids_setup(Apx);
         else, amg_gridA = amg_grids_setup(Bst*(Gdiag\Bst')); end
         fprintf('BinvGB setup done.\n')
         if damps==1
            fprintf('point damped Jacobi smoothing on finest level ..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PDJ', 2);
            elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PGS',2);
            else 
            fprintf('ILU smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'ILU',1);
          end
          amg_smootherA = amg_smoother_setup(amg_gridA, smoother_paramsA);       
            mfun_par = struct('Mfun','m_xbfbt_amgz','domain',domain,...
                              'Gdiag',Gdiag,'n_null',n_null);
         else
         error('LSC is not implemented for stabilized approximation!')
         end
         elseif precon==4,      %%% AMG iterated modified Fp
	     fprintf('AMG iterated PCD* preconditioning ...\n')  
         if n_null>0,
            Apx = pressurebc(Ap,n_null); 
            amg_gridA = amg_grids_setup(Apx);
         else
            amg_gridA = amg_grids_setup(Ap);
         end
	     fprintf('AMG setup of Ap done.\n')
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
         mfun_par = struct('Mfun','m_xfp_amgz','Fp',Fp,'G',G,'Mp', ...
                           Q,'domain',domain,'n_null',n_null); 
         else     %%%% precon==5, boundary-adjusted least squares 
         fprintf('AMG iterated boundary-adjusted LSC preconditioning ...\n');
         Gdiag = spdiags(diag(G),0,2*nv,2*nv);
         bcweights = m_bcscale(xy,domain,qmethod);
         weight = min(diag(bcweights));
         if qmethod>1,
            if n_null>0, 
               Apx = pressurebc(Bst*(Gdiag\Bst'),n_null);
               Apxw = pressurebc(Bst*(bcweights*(Gdiag\Bst')));
            else
               Apx = Bst*(Gdiag\Bst');
               Apxw = Bst*(bcweights*(Gdiag\Bst'));
            end
         elseif qmethod<=1,
            if qmethod==0,
               [Cp1,Cp2] = Cpre_q1q1(xy,ev);
            else
               [Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain);
               Cp1=beta*Cp1; Cp2=beta*Cp2;
            end
            if n_null>0, 
               Apx = pressurebc(Bst*(Gdiag\Bst')+Cp1,n_null);
               Apxw = pressurebc(Bst*(bcweights*(Gdiag\Bst'))+weight*Cp1,n_null);
            else
               Apx = Bst*(Gdiag\Bst')+Cp1;
               Apxw = Bst*(bcweights*(Gdiag\Bst'))+weight*Cp1;
            end
         end 
         Apx = Apx.*(abs(Apx)>5.d-15);
         Apxw = Apxw.*(abs(Apxw)>5.d-15);
         amg_gridA = amg_grids_setup(Apx);
         amg_gridAw = amg_grids_setup(Apxw);
	     fprintf('AMG setup of Ap and weighted Ap done.\n')
         if damps==1
            fprintf('point damped Jacobi smoothing on finest level ..\n')
		    smoother_paramsA = amg_smoother_params(amg_gridA, 'PDJ', 2);
            smoother_paramsAw = amg_smoother_params(amg_gridAw, 'PDG', 2);
         elseif damps==0
            fprintf('point GS smoothing on finest level..\n')
            smoother_paramsA = amg_smoother_params(amg_gridA, 'PGS');
            smoother_paramsAw = amg_smoother_params(amg_gridAw, 'PGS');
         else 
            fprintf('ILU smoothing on finest level..\n')
		    smoother_paramsA = amg_smoother_params(amg_gridA, 'ILU',1);
		    smoother_paramsAw = amg_smoother_params(amg_gridAw, 'ILU',1);
         end
         amg_smootherA = amg_smoother_setup(amg_gridA, smoother_paramsA);
         amg_smootherAw = amg_smoother_setup(amg_gridAw, smoother_paramsAw);
         if qmethod>1, 
            mfun_par = struct('Mfun','m_xbfbt_bc_amgz','domain',domain,...
                              'Gdiag',Gdiag,'n_null',n_null, 'weights',bcweights);  
         else
            mfun_par = struct('Mfun','m_sxbfbt_bc_amgz','domain',domain,...
                              'Gdiag',Gdiag,'viscosity',viscosity,...
                              'Cp2',Cp2,'n_null',n_null, 'weights',bcweights); 
         end
         end
   end
%
% solve using GMRES 
% zero initial guess
   rhs=[fzz;gzz]; x0=zeros(size(rhs)); 
   tic %%start timing 
      fprintf('\nGMRES iteration ...\n');
      params = [tol,maxit,1];
      [x_it,flag,iter,resvec] = gmres_r(afun_par,mfun_par,rhs,params,x0);
   etoc = toc;
end
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
