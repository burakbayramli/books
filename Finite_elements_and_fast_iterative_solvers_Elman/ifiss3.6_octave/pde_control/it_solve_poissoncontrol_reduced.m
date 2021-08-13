%IT_SOLVE_POISSONCONTROL_REDUCED solves (reduced) 2x2 block system
%system for the Poisson control problem
%IFISS scriptfile: JWP; DJS; 29 June 2012. 
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester

% Declare global variables for scalar and vector problems
global amg_grid amg_smoother number_vcycles
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% POISSON CONTROL PROBLEM
fprintf('discrete Poisson control system ...\n')
% use MINRES as iterative method
fprintf('iterative solution with preconditioned MINRES\n')
%
% set parameters
tol = default('tolerance? (default 1e-6)',1e-6);
maxit = default('maximum number of iterations? (default 100)',100);
%
% select preconditioner and construct it
fprintf('preconditioner:\n');
fprintf('   0   none\n');
fprintf('   1   diagonal preconditioner\n');
fprintf('   2   ideal block preconditioner, Schur comp. approx. 1\n');
fprintf('   3   ideal block preconditioner, Schur comp. approx. 2\n');
fprintf('   4   diagonal mass matrix approx., Schur comp. approx. 1, GMG\n');
fprintf('   5   diagonal mass matrix approx., Schur comp. approx. 1, AMG\n');
fprintf('   6   diagonal mass matrix approx., Schur comp. approx. 2, GMG\n');
fprintf('   7   diagonal mass matrix approx., Schur comp. approx. 2, AMG\n');
fprintf('   8   Chebyshev mass matrix approx., Schur comp. approx. 1, GMG\n');
fprintf('   9   Chebyshev mass matrix approx., Schur comp. approx. 1, AMG\n');
fprintf('   10  Chebyshev mass matrix approx., Schur comp. approx. 2, GMG\n');
fprintf('   11  Chebyshev mass matrix approx., Schur comp. approx. 2, AMG\n');
precon = default('preconditioner (default is Chebyshev, Schur comp. approx. 2, AMG)',11);
if precon==0,      % none
  fprintf('no preconditioning ...\n') 
  MA = 'mass_identity';
  MS = 'schur_identity';
elseif precon==1,  % diagonal
  fprintf('diagonal preconditioning ...\n') 
  D=diag((diag(K)./diag(M)).*diag(K)+1/beta*diag(M));
  MA = 'mass_diagonal_reduced'; massparams = struct('M',M);
  MS = 'schur_diagonal'; schurparams = struct('D',D);
elseif precon==2,  % block ideal, Schur comp. approx. 1
  fprintf('block (ideal) preconditioning with Schur comp. approx. 1 ...\n')
  MA = 'mass_exact_reduced'; massparams = struct('M',M);
  MS = 'schur_exact'; schurparams = struct('M',M,'L',K);
elseif precon==3,  % block ideal, Schur comp. approx. 2
  fprintf('block (ideal) preconditioning with Schur comp. approx. 2 ...\n')
  MA = 'mass_exact_reduced'; massparams = struct('M',M);
  MS = 'schur_exact'; schurparams = struct('M',M,'L',K+1/sqrt(beta)*M); 
elseif precon==4  % diagonal, Schur comp. approx. 1, GMG
  fprintf('diagonal mass matrix approx., Schur comp. approx. 1, GMG ...\n')
  MA = 'mass_diagonal_reduced'; massparams = struct('M',M);
  if problem==4
      domain = 2; % L-shaped domain
  else
      domain = 1; % square domain
  end
  A = K; % matrix we wish to solve for
  no_gmg = default('number of GMG V-cycles? (default 2)',2);
  if no_gmg<1 || fix(no_gmg)<no_gmg || fix(no_gmg)>no_gmg
      error('illegal parameter choice ''no_gmg'', try again.')
  end
  mg_poissoncontrol
  MS = 'schur_gmg';
  schurparams = struct('M',M,'mgdata',mgdata,'smooth_data',smooth_data, ...
     'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps,'no_gmg',no_gmg);
elseif precon==5 % diagonal, Schur comp. approx. 1, AMG
  fprintf('diagonal mass matrix approx., Schur comp. approx. 1, AMG ...\n')
  MA = 'mass_diagonal_reduced'; massparams = struct('M',M);
  number_vcycles = default('number of AMG V-Cycles? (default 2)',2);
  amg_grid = amg_grids_setup(K);
  sm_ch = default('AMG smoother; point Jacobi (1), point Gauss-Seidel (2) or ILU (3)? (default 1)',1);
  if sm_ch==1
      smoother_type = 'PDJ';
  elseif sm_ch==2
      smoother_type = 'PGS';
  elseif sm_ch==3
      smoother_type = 'ILU';
  else
      error('illegal parameter choice ''smoother_type'', try again.')
  end
  no_sweeps = default('number of pre- and post-smoothing steps? (default 2)',2);
  if no_sweeps>=0 && fix(no_sweeps)==no_sweeps
      smoother_params = amg_smoother_params(amg_grid,smoother_type,no_sweeps);
  else
      error('illegal parameter choice ''no_sweeps'', try again.')
  end
  amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
  MS = 'schur_amg'; schurparams = struct('M',M,'A',K); 
elseif precon==6 % diagonal, Schur comp. approx. 2, GMG
  fprintf('diagonal mass matrix approx., Schur comp. approx. 2, GMG ...\n')
  MA = 'mass_diagonal_reduced'; massparams = struct('M',M);
  if problem==4
      domain = 2; % L-shaped domain
  else
      domain = 1; % square domain
  end
  A = K+1/sqrt(beta)*M; % matrix we wish to solve for
  no_gmg = default('number of GMG V-cycles? (default 2)',2);
  if no_gmg<1 || fix(no_gmg)<no_gmg || fix(no_gmg)>no_gmg
      error('illegal parameter choice ''no_gmg'', try again.')
  end
  mg_poissoncontrol
  MS = 'schur_gmg';
  schurparams = struct('M',M,'mgdata',mgdata,'smooth_data',smooth_data, ...
     'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps,'no_gmg',no_gmg);
elseif precon==7 % diagonal, Schur comp. approx. 2, AMG
  fprintf('diagonal mass matrix approx., Schur comp. approx. 2, AMG ...\n')
  MA = 'mass_diagonal_reduced'; massparams = struct('M',M);
  number_vcycles = default('number of AMG V-Cycles? (default 2)',2);
  amg_grid = amg_grids_setup(K+1/sqrt(beta)*M);
  sm_ch = default('AMG smoother; point Jacobi (1), point Gauss-Seidel (2) or ILU (3)? (default 1)',1);
  if sm_ch==1
      smoother_type = 'PDJ';
  elseif sm_ch==2
      smoother_type = 'PGS';
  elseif sm_ch==3
      smoother_type = 'ILU';
  else
      error('illegal parameter choice ''smoother_type'', try again.')
  end
  no_sweeps = default('number of pre- and post-smoothing steps? (default 2)',2);
  if no_sweeps>=0 && fix(no_sweeps)==no_sweeps
      smoother_params = amg_smoother_params(amg_grid,smoother_type,no_sweeps);
  else
      error('illegal parameter choice ''no_sweeps'', try again.')
  end
  amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
  MS = 'schur_amg'; schurparams = struct('M',M,'A',K+1/sqrt(beta)*M);
elseif precon==8,  % Chebyshev, Schur comp. approx. 1, GMG
  fprintf('Chebyshev mass matrix approx., Schur comp. approx. 1, GMG ...\n')
  cheb_its = default('number of Chebyshev iterations? (default 10)',10);
  % Call mass matrix 'Q', not 'M', below to agree with structure of code 
  MA = 'mass_chebyshev_reduced';
  massparams = struct('Q',M,'its',cheb_its,'qmethod',qmethod);
  if problem==4
      domain = 2; % L-shaped domain
  else
      domain = 1; % square domain
  end
  A = K; % matrix we wish to solve for
  no_gmg = default('number of GMG V-cycles? (default 2)',2);
  if no_gmg<1 || fix(no_gmg)<no_gmg || fix(no_gmg)>no_gmg
      error('illegal parameter choice ''no_gmg'', try again.')
  end
  mg_poissoncontrol
  MS = 'schur_gmg';
  schurparams = struct('M',M,'mgdata',mgdata,'smooth_data',smooth_data, ...
     'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps,'no_gmg',no_gmg);
elseif precon==9,  % Chebyshev, Schur comp. approx. 1, AMG
  fprintf('Chebyshev mass matrix approx., Schur comp. approx. 1, AMG ...\n')
  cheb_its = default('number of Chebyshev iterations? (default 10)',10);
  % Call mass matrix 'Q', not 'M', below to agree with structure of code 
  MA = 'mass_chebyshev_reduced';
  massparams = struct('Q',M,'its',cheb_its,'qmethod',qmethod);
  number_vcycles = default('number of AMG V-Cycles? (default 2)',2);
  amg_grid = amg_grids_setup(K);
  sm_ch = default('AMG smoother; point Jacobi (1), point Gauss-Seidel (2) or ILU (3)? (default 1)',1);
  if sm_ch==1
      smoother_type = 'PDJ';
  elseif sm_ch==2
      smoother_type = 'PGS';
  elseif sm_ch==3
      smoother_type = 'ILU';
  else
      error('illegal parameter choice ''smoother_type'', try again.')
  end
  no_sweeps = default('number of pre- and post-smoothing steps? (default 2)',2);
  if no_sweeps>=0 && fix(no_sweeps)==no_sweeps
      smoother_params = amg_smoother_params(amg_grid,smoother_type,no_sweeps);
  else
      error('illegal parameter choice ''no_sweeps'', try again.')
  end
  amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
  MS = 'schur_amg'; schurparams = struct('M',M,'A',K);
elseif precon==10,  % Chebyshev, Schur comp. approx. 2, GMG
  fprintf('Chebyshev mass matrix approx., Schur comp. approx. 2, GMG ...\n')
  cheb_its = default('number of Chebyshev iterations? (default 10)',10);
  % Call mass matrix 'Q', not 'M', below to agree with structure of code 
  MA = 'mass_chebyshev_reduced';
  massparams = struct('Q',M,'its',cheb_its,'qmethod',qmethod);
  if problem==4
      domain = 2; % L-shaped domain
  else
      domain = 1; % square domain
  end
  A = K+1/sqrt(beta)*M; % matrix we wish to solve for
  no_gmg = default('number of GMG V-cycles? (default 2)',2);
  if no_gmg<1 || fix(no_gmg)<no_gmg || fix(no_gmg)>no_gmg
      error('illegal parameter choice ''no_gmg'', try again.')
  end
  mg_poissoncontrol
  MS = 'schur_gmg';
  schurparams = struct('M',M,'mgdata',mgdata,'smooth_data',smooth_data, ...
     'nc',nc,'npre',npre,'npost',npost,'sweeps',sweeps,'no_gmg',no_gmg);
elseif precon==11,  % Chebyshev, Schur comp. approx. 2, AMG
  fprintf('Chebyshev mass matrix approx., Schur comp. approx. 2, AMG ...\n')
  cheb_its = default('number of Chebyshev iterations? (default 10)',10);
  % Call mass matrix 'Q', not 'M', below to agree with structure of code 
  MA = 'mass_chebyshev_reduced';
  massparams = struct('Q',M,'its',cheb_its,'qmethod',qmethod);
  number_vcycles = default('number of AMG V-Cycles? (default 2)',2);
  amg_grid = amg_grids_setup(K+1/sqrt(beta)*M);
  sm_ch = default('AMG smoother; point Jacobi (1), point Gauss-Seidel (2) or ILU (3)? (default 1)',1);
  if sm_ch==1
      smoother_type = 'PDJ';
  elseif sm_ch==2
      smoother_type = 'PGS';
  elseif sm_ch==3
      smoother_type = 'ILU';
  else
      error('illegal parameter choice ''smoother_type'', try again.')
  end
  no_sweeps = default('number of pre- and post-smoothing steps? (default 2)',2);
  if no_sweeps>=0 && fix(no_sweeps)==no_sweeps
      smoother_params = amg_smoother_params(amg_grid,smoother_type,no_sweeps);
  else
      error('illegal parameter choice ''no_sweeps'', try again.')
  end
  amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
  MS = 'schur_amg'; schurparams = struct('M',M,'A',K+1/sqrt(beta)*M);
else
  error('illegal parameter choice ''precon'', try again.')
end

%
% zero initial guess
% x0=zeros(size(rhs));
tic %%start timing
fprintf('\nMINRES iteration ...\n');
if precon==0, 
   [x_it,resvec,iter,flag] = poissoncontrol_minres_reduced(M,K,beta, ...
       Myhat,d,maxit,tol,MA,[],MS,[]);
else
   [x_it,resvec,iter,flag] = poissoncontrol_minres_reduced(M,K,beta, ...
       Myhat,d,maxit,tol,MA,massparams,MS,schurparams);
end
etoc = toc;
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
   fprintf('Sweet!\n') % fprintf('Bingo!\n')
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

