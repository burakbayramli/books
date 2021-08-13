%IT_SOLVE iterative solution of predefined steady problem
%IFISS scriptfile: DJS; HCE; AR; 9 January 2019.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 

% Declare global variables for scalar and vector problems
global amg_grid amg_smoother   
global amg_gridA amg_smootherA amg_gridF amg_smootherF amg_gridAw amg_smootherAw

if exist('pde','var')==0,
   error('You need to set up a specific discrete problem first!'), 
end
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
if pde==1,
   %%% DIFFUSION PROBLEM
   fprintf('discrete diffusion system ...\n')

   % select Krylov subspace method
   itmeth = default('PCG/MINRES? 1/2 (default PCG)',1);

   % set parameters
   tol = default('tolerance? (default 1e-6)',1e-6);
   maxit = default('maximum number of iterations? (default 100)',100);
   
   % select preconditioner and construct it
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   1  diagonal\n');
   fprintf('   2  incomplete cholesky\n');
   fprintf('   3  geometric multigrid\n');
   fprintf('   4  algebraic multigrid\n');
   precon = default('default is AMG ',4);
   if precon==0,     % none
      M1=[]; M2=[];
   elseif precon==1, % diagonal
      D=diag(diag(Agal)); M1=sqrt(D); M2=M1; 
   elseif precon==2, % incomplete Cholesky
     M1 = ichol(Agal); M2=M1';
%    M2 = cholinc(Agal,'0'); M1=M2'; fprintf('--- cholinc.m\n')%
   elseif precon==3, % MG
      if (domain==1 || domain==2), mg_diff,
      else  error('MG for this problem/domain is not available!')
      end
      M1 = 'm_mg'; M2 = []; 
      mparams=struct('mgdata',mgdata,'smooth_data',smooth_data, ...
                     'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps);
   elseif precon==4, % AMG
 % uses global variables amg_grid amg_smoother
      amg_grid = amg_grids_setup(Agal);
      fprintf('\nsetup done.\n')
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
         [x_it,flag,relres,iter,resvec] = pcg(Agal,fgal,tol,maxit,M1,M2,x0);
      elseif precon==3
         [x_it,flag,relres,iter,resvec] = ...
            pcg(Agal,fgal,tol,maxit, M1,M2,x0,[],mparams);
      elseif  precon==4
          [x_amg,flag,relres,iter,resvec] = ...
             pcg(Agal,fgal,tol,maxit, @amg_v_cycle, [], x0, amg_grid, amg_smoother); 
      end
   elseif itmeth==2, %MINRES
      fprintf('\nMINRES iteration ...\n');
      if precon<=2, 
         [x_it,flag,relres,iter,resvec] = minres(Agal,fgal,tol,maxit,M1,M2,x0);
      elseif precon==3
         [x_it,flag,relres,iter,resvec] = ...
            minres(Agal,fgal,tol,maxit,M1,M2,x0,[],mparams);
      elseif  precon==4
           [x_it,flag,relres,iter,resvec] = ...
              minres(Agal,fgal,tol,maxit,@amg_v_cycle, [], x0, amg_grid, amg_smoother);
      end
   else
      error('invalid iterative method!')
   end
   etoc = toc;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
elseif pde==2,
   %%% CONVECTION-DIFFUSION PROBLEM
   fprintf('discrete convection-diffusion system ...\n\n')
% 
   % set structure for matrix-vector product   
   afun_par = struct('Afun','a_cdt','A',Asupg);
%
   % select Krylov subspace method and set parameters 
   itmeth = default('GMRES/Bicgstab(l)/IDR(s)  1/2/3 (default GMRES)',1);
   if ( itmeth == 2 )
      ell = default('ell? (default 2)',2);
   elseif ( itmeth == 3 )
      ess = default('s? (default 4)',4);
   end
   tol = default('stopping tolerance? (default 1e-6)',1e-6);
   maxit = default('maximum number of iterations? (default 100)',100);
% 
   % select preconditioner and construct it
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   1  diagonal\n');
   fprintf('   2  incomplete LU\n');
   fprintf('   3  geometric multigrid\n');
   fprintf('   4  algebraic multigrid\n');
   precon = default('default is AMG ',4);
   if precon==0,      % none
      mfun_par=struct('Mfun','m_nonet');
   elseif precon==1,  % diagonal
      D=diag(diag(Asupg)); 
      mfun_par=struct('Mfun','m_diagt','D',D);
   elseif precon==2,  % incomplete LU
      setup.type='nofill';
      %[L,U]=ilu0(Asupg);fprintf('--- iluo.m\n')%
      [L,U]=ilu(Asupg,setup); 
      mfun_par=struct('Mfun','m_ilut','L',L,'U',U);
   elseif precon==3,  % MG
      mg_cd
      mfun_par = struct('Mfun','m_mg','mgdata',mgdata,'smooth_data',smooth_data, ...
         'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps);
   elseif precon==4,  % AMG
%  uses global variables amg_grid amg_smoother
%% set up structure for matrix-vector multiply   
      afun_par = struct('Afun','a_cdt','A',Asupg);
  %% 
  % compute new MG data or reload existing data?
      compute_mg = default('compute / load AMG data? 1/2 (default 1)',1);
      if compute_mg==2
         load amgdata_cd.mat
      else
         amg_grid = amg_grids_setup(Asupg);
         fprintf('\nsetup done.\n')
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
% solve using GMRES or BiCGSTAB(ell) or IDR(s)
% zero initial guess
   x0=zeros(size(fsupg));
   tic %%start timing
   if itmeth==1, %GMRES
      fprintf('\nGMRES iteration ...\n');
      params = [tol,maxit,1];
      [x_it,flag,iter,resvec] = gmres_r(afun_par,mfun_par,fsupg,params,x0);
    elseif itmeth==2, %BiCGSTAB(ell)
      fprintf('BiCGSTAB(%2i) iteration ...\n',ell);
      params = [tol,maxit,ell,.7];
      [x_it,flag,iter,resvec] = bicgstab_ell_r(afun_par,mfun_par,fsupg,params,x0);
   elseif itmeth==3, %IDR(S)
      fprintf('IDR(%2i) iteration ...\n', ess);
      params = [tol,maxit,ess,.7];
      [x_it,flag,iter,resvec] = idrs_r(afun_par,mfun_par,fsupg,params,x0);
   else
      error('invalid iterative method!')
   end
   etoc = toc;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
elseif pde==3
   %%% STOKES PROBLEM
   fprintf('discrete Stokes system ...\n')
   % use MINRES as iterative method
   fprintf('iterative solution with preconditioned MINRES\n')
%
   % set parameters
   tol = default('tolerance? (default 1e-6)',1e-6);
   maxit = default('maximum number of iterations? (default 100)',100);
%
   % specify coefficient matrix
   if qmethod<=1, stokes_mat = [Ast,Bst';Bst,-beta*C];
   else           stokes_mat = [Ast,Bst';Bst,sparse(np,np)];
   end
%
   % select preconditioner and construct it
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   1  diagonal\n');
   fprintf('   2  ideal block\n');
   fprintf('   4  AMG block\n');
   precon = default('default is AMG ',4);
   if precon==0,      % none
      fprintf('no preconditioning ...\n') 
      M1=[]; M2=[];  
   elseif precon==1,  % diagonal
      fprintf('diagonal preconditioning ...\n') 
      D=diag([diag(Ast); diag(Q)]); M1=sqrt(D); M2=M1; 
   elseif precon==2,  % block
      fprintf('block (ideal) preconditioning ...\n')
      M1 = 'm_st_block'; M2 = [];
      mparams = struct('Ast',Ast,'Q',Q);
   elseif precon==4; %AMG
%     uses global variables global amg_grid amg_smoother
      fprintf('AMG preconditioning...\n') 
      nv=size(Ast,1); np=size(Q,1); nu=nv/2;
	  Agal=Ast(1:nu,1:nu);
      % set up AMG structure
      amg_grid = amg_grids_setup(Agal);
      fprintf('setup done.\n')
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
	  error('invalid preconditioner!');
   end
%
   fprintf('\nMINRES iteration ...\n');   
   % zero initial guess
   x0=zeros(size([fst;gst]));
   tic %%start timing
   if precon<=1,
      [x_it,flag,relres,iter,resvec] = ...
         minres(stokes_mat,[fst;gst],tol,maxit, M1,M2,x0);
   elseif precon<=3,
      [x_it,flag,relres,iter,resvec] = ...
         minres(stokes_mat,[fst;gst],tol,maxit, M1,M2,x0,mparams);
   elseif precon==4,
       if qmethod<=1
	     [x_amg,flag,relres,iter,resvec]= ...
	     minres([Ast,Bst';Bst,-beta*C],[fst;gst],tol,maxit, ...
                    'm_st_amgz',[],x0,Ast,Q);
       else
	     [x_amg,flag,relres,iter,resvec]= ...
	     minres([Ast,Bst';Bst,sparse(np,np)],[fst;gst],tol,maxit, ...
                    'm_st_amgz',[],x0,Ast,Q);
       end   
   end
   etoc = toc;
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
elseif pde==4
   %%% NAVIER-STOKES Problem
   if     domain==1,  
      fprintf('enclosed flow (cavity) problem ...\n')
   elseif domain==3,  
      fprintf('inflow/outflow (step) problem ...\n')      
   elseif domain==4,
      fprintf('inflow/outflow (obstacle) problem ...\n')
   else 
      error('solvers for this problem/domain are not available!');
   end
%
   % set structure for matrix-vector product
   if nlmethod==0,
      % use Oseen matrix from just completed Picard iteration
      fprintf('\nsolving Oseen system generated by solution from last Picard step\n\n')
      if qmethod>1,
         afun_par = struct('Afun','a_nst','F',Anst,'B',Bst,'D',sparse(np,np));
      else
         afun_par = struct('Afun','a_nst','F',Anst,'B',Bst,'D',-nubeta*C);
      end
   else      
      % use Jacobian of current solution 
      fprintf('\nsolving Jacobian system generated by solution from last Newton step\n\n')
      if     qmethod>1,  [Nxx,Nxy,Nyx,Nyy] = newton_q2(xy,mv,flowsol);   
      elseif qmethod<=1, [Nxx,Nxy,Nyx,Nyy] = newton_q1(xy,ev,flowsol);
      end
      J = viscosity*A + [N + Nxx, Nxy; Nyx, N + Nyy];
      Jnst = newtonbc(J,xy,bound); 
      if qmethod>1, 
         afun_par = struct('Afun','a_nst','F',Jnst,'B',Bst,'D',sparse(np,np));
      else
         afun_par = struct('Afun','a_nst','F',Jnst,'B',Bst,'D',-nubeta*C);
      end
   end
%
   % select Krylov subspace method and set parameters 
   itmeth = default('GMRES/Bicgstab(l)/IDR(s)  1/2/3 (default GMRES)',1);
   if ( itmeth == 2 )
      ell = default('ell? (default 2)',2);
   elseif ( itmeth == 3 ) 
      ess = default('s? (default 4)',4);
   end 
   tol = default('stopping tolerance? (default 1e-6)',1e-6);
   maxit = default('maximum number of iterations? (default 100)',100);
   
   % select preconditioner
   fprintf('preconditioner:\n');
   fprintf('   0  none\n');
   fprintf('   1  unscaled least-squares commutator (BFBt)\n');
   fprintf('   2  pressure convection-diffusion (Fp)\n');
   fprintf('   3  least-squares commutator (LSC)\n');
   fprintf('   4  modified pressure convection-diffusion (Fp*)\n');
   fprintf('   5  boundary-adjusted least-squares commutator (LSC*)\n');
   precon = default('default is modified pressure convection-diffusion',4);
   if precon>0,
      query = 'ideal / AMG iterated  preconditioning? 1/2 (default ideal)';
      precon_format = default(query,1);
      if (precon_format>2 ),
	     error(' GMG for Navier-Stokes is not implemented in IFISS3.0!')
      end
   else
      precon_format = 1;
   end
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
   x0=zeros(size([fst;gst]));
   tstart = tic; %%start timing
   if itmeth==1, %GMRES 
      fprintf('\nGMRES iteration ...\n');
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
   etoc = toc(tstart);
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
   %%% plot residuals
   resplot(resvec)
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
