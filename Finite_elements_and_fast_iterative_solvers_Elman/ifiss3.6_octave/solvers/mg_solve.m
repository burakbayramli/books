%MG_SOLVE driver for GMG solution of predefined problem 
%IFISS scriptfile: AR, HCE, DJS; 27 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

if  exist('pde','var')==0,
	error('You need to set up a specific discrete problem first!'), end
if  exist('resvec','var')==1, clear resvec, end

nc=log2(length(x)-1);

% compute new MG data or reload existing data?
compute_mg = default('compute / load MG data? 1/2 (default 1)',1);

if pde==1
   fprintf('discrete diffusion system...\n')
   f=fgal;
   if compute_mg==2
      load mgdata_diff.mat
   else
      if domain==1 % square domain
         h=2^(1-nc);
         fprintf('Setting up MG data ...')
         % top level
         mgdata(nc).matrix=Agal;
         mgdata(nc).prolong=mg_prolong(2^nc,2^nc,x,y);
         xn=x(1:2:end);yn=y(1:2:end);
         % loop over remaining levels
         for level = nc-1:-1:1;
            % matrix
            mgdata(level).matrix=mg_diff_setup(xn,yn);
            % prolongation operator
            mgdata(level).prolong=mg_prolong(2^level,2^level,xn,yn);
            % coarsen coordinates
            xn=xn(1:2:end);yn=yn(1:2:end);
         end
         fprintf('done\n')
         gohome
         cd datafiles
         save mgdata_diff.mat mgdata
      elseif domain==2 % L-shaped domain
         x=x';y=y'; 
         h=2^(1-nc);
         fprintf('Setting up MG data ...')
         % top level
         mgdata(nc).matrix=Agal;
         mgdata(nc).prolong=mg_prolong_ell(nc,x,y);
         xn=x;yn=y;
         % loop over remaining levels
         for level = nc-1:-1:2;
            % coarsen coordinates
            xn=xn(1:2:end);yn=yn(1:2:end);
            % coefficient matrix
            mgdata(level).matrix=mg_diff_setup_ell(xn,yn);
            % prolongation operator
            mgdata(level).prolong=mg_prolong_ell(level,xn,yn);
         end
         fprintf('done\n')
         gohome
         cd datafiles
         save mgdata_diff.mat mgdata
         x=x';y=y';
      end
   end
elseif pde==2
   fprintf('discrete convection-diffusion system...\n')
   f=fsupg;
   if compute_mg==2
      load mgdata_cd.mat
   else
      h=2^(1-nc);
      fprintf('Setting up MG data ...')
      % top level
      mgdata(nc).matrix=Asupg;
      mgdata(nc).prolong=mg_prolong(2^nc,2^nc,x,y);
      xn=x(1:2:end);yn=y(1:2:end);
      % loop over remaining levels
      for level = nc-1:-1:1;
         % coefficient matrix
         mgdata(level).matrix=mg_cd_setup(xn,yn,viscosity,outbc);
         % prolongation operator
         mgdata(level).prolong=mg_prolong(2^level,2^level,xn,yn);
         % coarsen coordinates
         xn=xn(1:2:end);yn=yn(1:2:end);
      end
      fprintf('done\n')
      gohome
      cd datafiles
      save mgdata_cd.mat mgdata
   end
else
	error('Multigrid solver not implemented for this PDE')
end

% MG parameters
tau = default('MG tolerance? (default 1e-6)',1e-6);
maxit = default('max. no. of iterations? (default 100)',100);
if pde==1
   smooth = default('Jacobi / ILU smoother? 1/3 (default is ILU)',3);
else
   smooth = default('Jacobi / Gauss-Seidel / ILU smoother? 1/2/3 (default is Gauss-Seidel)',2);
end
if smooth==3
   % point ILU
   sweeps=1;stype=1;
elseif smooth==2
   % Gauss-Seidel
   stype = default('point/line Gauss-Seidel? 1/2 (default is line)',2);
   if stype==2
      sweeps = default('number of Gauss-Seidel sweeps? 1/2/3/4 (default is 2)',2);   else
      sweeps=1;
   end
else
   % point Jacobi
   sweeps=1;stype=1;
end
npre = default('number of pre-smoothing steps? (default is 1)',1);
npost = default('number of post-smoothing steps? (default is 1)',1);

% MG set-up
A = mgdata(nc).matrix;
x_mg = zeros(length(f),1);
r = f;
nr0 = norm(r);
nr = nr0;
resvec(1)=nr0;
its = 0;
stats = [its,nr];

% construct smoother 
if domain==2
   % L-shaped
   smooth_data = mg_smooth_ell(mgdata,nc,sweeps,smooth,stype);
else
   smooth_data = mg_smooth(mgdata,nc,sweeps,smooth,stype);
end

% MG iterative loop
flag=0;
iter=0;
tic %%start timing
while (nr>tau*nr0)&(iter<=maxit)
   c = mg_iter(mgdata,zeros(size(r)),r,smooth_data,nc,npre,npost,sweeps);
   x_mg = x_mg+c;
   r = f - A*x_mg;
   nr = norm(r);
   iter = iter+1;
   resvec(iter+1)=nr;
end
if iter==(maxit+1)|isnan(nr)
   flag=1;
end
etoc=toc;
%tats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
   %%% plot residual
   resplot(resvec)
else
   nr0=resvec(1);
   fprintf('\n    k  log10(||r_k||/||r_0||)   \n')
   for its=1:iter+1,
      fprintf('%5i %16.4f \n', its-1, log10(resvec(its)/nr0));
   end
   fprintf('iteration aborted! Iteration returned with flag equal to %2i \n',flag)
   %%% plot residuals
   resplot(resvec)
end
