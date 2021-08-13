function x_it = it_nstokes(Anst,Bst,C,nubeta,Q,G,nlres,flowsol,...
                           viscosity,itslv_params, geo_domain)
%IT_NSTOKES iterative solver for flow linear system
%  IFISS function: HCE; 5 June 2013; DJS; 20 Oct 2013.
% Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage

% get geometry information
   if geo_domain==1,
      load square_stokes_nobc.mat,
   elseif geo_domain==3,
      load step_stokes_nobc.mat
   else
      error('iterative solvers for this problem/domain are not available!');
   end
%
   if domain ~= geo_domain
      error('inconsistent domain information in it_nstokes.m');
   end
%
% set parameters for iteration
   itmeth        = itslv_params.itmeth;
   tol           = itslv_params.tol;
   maxit         = itslv_params.maxit;
   precon        = itslv_params.precon;
   precon_format = itslv_params.precon_format; 
%
%%% NAVIER-STOKES Problem
   if     domain==1,  
      fprintf('enclosed flow (cavity) problem ...\n')
   elseif domain==3,  
      fprintf('inflow/outflow (step) problem ...\n')      
   else 
      error('solvers for this problem/domain are not available!');
   end
%  
% set structure for matrix-vector product
   afun_par = struct('Afun','a_nst','F',Anst,'B',Bst,'D',-nubeta*C);
%
% set structure for preconditioner
   n_null = null_pressure_index(domain,qmethod,size(Bst,1));
   if precon==2,      %%%%  Fp: need construction of Ap and Fp blocks
      if qmethod==1,
         %% set up Fp operator for Q0 approximation	   
         [Ap,Fp] = fpsetup_q0(xy,xyp,ev,ee,flowsol,viscosity,domain);	
      elseif qmethod==2
	     %% set up Fp operator for Q1 approximation	   
         [Ap,Fp] = fpsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain); 
      elseif qmethod==3
	      %% set up Fp operator for P1 approximation	   
          [Ap,Fp] = fpsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain);
      elseif qmethod==4
	     %% set up Fp operator for Q0 approximation	   
         [Ap,Fp] = fpsetup_q2p0(xy,xyp,mv,ee,flowsol,viscosity,domain);
      elseif qmethod==0
         %% set up Fp operator for Q1 approximation
         [Ap,Fp] = fpsetup_q1(xy,xyp,ev,ev,flowsol,viscosity,domain);
      end
   elseif precon==4,      %%%%  modified Fp: need construction of Ap and Fp blocks 
      if qmethod==1,
        %% set up Fp* operator for Q0 approximation	 
		 [Ap,Fp] = fpzsetup_q0(xy,xyp,ev,ee,flowsol,viscosity,domain);
         %error('Not yet implemented!')
      elseif qmethod==2
         %% set up Fp* operator for Q1 approximation	   
         [Ap,Fp] = fpzsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain,map);
      elseif qmethod==3
	     %% set up Fp* operator for P1 approximation	   
         [Ap,Fp] = fpzsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain);
      elseif qmethod==4
	     %% set up Fp* operator for Q0 approximation	   
         error('Not yet implemented!')
      elseif qmethod==0
         %% set up Fp* operator for Q1 approximation
         np=length(xyp(:,1));
	     [Ap,Fp] = fpzsetup_q1(xy,xyp,ev,ev,flowsol,viscosity,domain,(1:np)');
         %error('Not yet implemented!')
      end
   end
   if precon_format==1, 
   % ideal preconditioning: use direct sparse elimination to solve block systems 
   % in preconditioner
      if precon==0,       %%% no preconditioning
         fprintf('no preconditioning ...\n')
         mfun_par=struct('Mfun','m_nonet');
      elseif precon==1,   %%% old (unscaled) least squares (BFBt) 
         fprintf('ideal unscaled least-squares commutator preconditioning ...\n')
         if qmethod>=2  
            mfun_par = struct('Mfun','m_bfbt','domain',domain,'n_null',n_null);
         else 
            error('least squares commutator is not defined for stabilized approximation!')
         end
      elseif precon==2,   %%%%  pressure commutator (Fp) 
         fprintf('ideal pressure convection-diffusion preconditioning ...\n')
         mfun_par = struct('Mfun','m_fp','Fp',Fp,'Ap',Ap,'Mp',Q,'domain',domain,'n_null',n_null);
      elseif precon==3    %%%% (scaled) least squares (BFBt)
         fprintf('ideal least-squares commutator preconditioning ...\n')
         if qmethod>1
            mfun_par = struct('Mfun','m_xbfbt','domain',domain,'G',G,'n_null',n_null);
         elseif qmethod==0,
            [Cp1,Cp2] = Cpre_q1q1(xy,ev);
            mfun_par = struct('Mfun','m_sxbfbt','domain',domain,'G',G,'Q',Q,...
                               'viscosity',viscosity,'Cp1',Cp1,'Cp2',Cp2,'n_null',n_null); 
         elseif qmethod==1,
           [Cp1,Cp2] = Cpre_q1p0(xy,xyp,ev,domain);
           Cp1=beta*Cp1; Cp2=beta*Cp2;
           mfun_par = struct('Mfun','m_sxbfbt','domain',domain,'G',G,'Q',Q,...
                             'viscosity',viscosity,'Cp1',Cp1,'Cp2',Cp2,'n_null',n_null); 
         end
      elseif precon==4 ,  %%%%  modified pressure commutator (Fp) 
         fprintf('modified pressure convection-diffusion preconditioning ...\n');
		 mfun_par = struct('Mfun','m_xfp','Fp',Fp,'Ap',Ap,'Mp',Q,'domain',domain,'n_null',n_null);
      else     %%%% precon==5, boundary-adjusted least squares commutator
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
%
   else
% precon_format==2, iterated preconditioning with algebraic multigrid 
%    uses global variables amg_gridA amg_smootherA amg_gridF amg_smootherF
%    set up velocity convection-diffusion structures
%    compute new MG data or reload existing data?
      compute_mg = default('compute / load convection-diffusion AMG data? 1/2 (default 1)',1);
      if compute_mg==2
         gohome, cd datafiles, load amgdata_cd.mat
      else
         amg_gridF = amg_grids_setup(Anst);
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
      elseif precon==2      %%% AMG iterated Fp
         fprintf('AMG iterated PCD preconditioning ...\n')              
         if n_null>0,  
            Apx = pressurebc(Ap,n_null); 
            amg_gridA = amg_grids_setup(Apx);
         else
            amg_gridA = amg_grids_setup(Ap); 
         end
         fprintf('\nPressure Poisson setup done.\n')
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
         mfun_par = struct('Mfun','m_fp_amgz','Fp',Fp,'Ap',Ap,'Mp', ...
                           Q,'domain',domain,'n_null',n_null);       
      elseif precon==3, %%% AMG iterated scaled BFBt 
         fprintf('AMG iterated LSC preconditioning ...\n')
         Gdiag=spdiags(diag(G),0,nnv,nnv);
         if qmethod>1,
            if n_null>0, 
               Apx = pressurebc(Bst*(Gdiag\Bst'),n_null);
            else
               Apx = Bst*(Gdiag\Bst');
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
            else
               Apx = Bst*(Gdiag\Bst')+Cp1;
            end
         end 
         amg_gridA = amg_grids_setup(Apx);
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
         if qmethod>1, 
            mfun_par = struct('Mfun','m_xbfbt_amgz','domain',domain,...
                              'Gdiag',Gdiag,'n_null',n_null);  
         else
            mfun_par = struct('Mfun','m_sxbfbt_amgz','domain',domain,...
                              'Gdiag',Gdiag,'viscosity',viscosity,...
                              'Cp2',Cp2,'n_null',n_null); 
         end
      elseif precon==4,      %%% AMG iterated boundary-adjusted Fp
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
         Gdiag = spdiags(diag(G),0,nnv,nnv);
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
%
% solve using GMRES or BiCGSTAB(ell) or IDR(s)
% zero initial guess
   x0=zeros(size(nlres));
   tic %%start timing
   if itmeth==1, %GMRES 
      fprintf('GMRES iteration ... ');
      params = [tol,maxit,1];
      [x_it,flag,iter,resvec] = gmres_r(afun_par,mfun_par,nlres,params,x0);
   elseif itmeth==2, %BiCGSTAB(ell)
      fprintf('BiCGSTAB(%2i) iteration ...\n',ell);
      params = [tol,maxit,ell,0.7];  
      [x_it,flag,iter,resvec] = bicgstab_ell_r(afun_par,mfun_par,nlres,params,x0);
   elseif itmeth==3, %IDR(S)
      fprintf('IDR(%2i) iteration ...\n', ess);
      params = [tol,maxit,ess,0.7];
      [x_it,flag,iter,resvec] = idrs_r(afun_par,mfun_par,nlres,params,x0);
   end
   etoc = toc;
%
%
% Print and plot results
if flag==0,
   % successful convergence
   fprintf('convergence in %3i iterations\n\n',iter)
else
   fprintf('iteration aborted! Iteration returned with flag equal to  %2i \n',flag)
   fprintf('maximum iteration count of %3i reached\n',maxit);
   fprintf('relative residual is %e\n',resvec(length(resvec))/resvec(1)); 
   %%% plot residuals
end
