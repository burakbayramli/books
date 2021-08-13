%MG_DIFF GMG preconditioner for diffusion problem
%IFISS scriptfile: AR, HCE, DJS; 27 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nc=log2(length(y)-1);
%
% compute new MG data or reload existing data?
compute_mg = default('compute / load MG data? 1/2 (default 1)',1);
if compute_mg==2
   load mgdata_diff.mat
else
   h=2^(1-nc);
   fprintf('Setting up MG data ...')
   % top level
   mgdata(nc).matrix=Agal;
   if domain==1 % square
      mgdata(nc).prolong=mg_prolong(2^nc,2^nc,x,y);
   elseif domain==2 % L-shaped
      x=x';y=y';
      mgdata(nc).prolong=mg_prolong_ell(nc,x,y);
   elseif domain==3 % step
      x=x';y=y';
      mgdata(nc).prolong=mg_prolong_step(nc,x,y);
   else
      error('Multigrid solver not implemented for this domain')
   end
   xn=x;yn=y;
   % loop over remaining levels
   for level = nc-1:-1:2;
      xn=xn(1:2:end);yn=yn(1:2:end);
      if domain==1 % square
         mgdata(level).matrix=mg_diff_setup(xn,yn);
         mgdata(level).prolong=mg_prolong(2^level,2^level,xn,yn);
      elseif domain==2
         mgdata(level).matrix=mg_diff_setup_ell(xn,yn);
         mgdata(level).prolong=mg_prolong_ell(level,xn,yn);
      else
         mgdata(level).matrix=mg_diff_setup_step(xn,yn);
         mgdata(level).prolong=mg_prolong_step(level,xn,yn);
      end
   end
   fprintf('done\n')
   gohome, cd datafiles, save mgdata_diff.mat mgdata
   if domain~=1, x=x';y=y';, end
end
%
% MG parameters
smooth = default('Jacobi / ILU smoother? 1/3 (default is ILU)',3);
if smooth==3
   % point ILU
   sweeps=1;stype=1;
elseif smooth==2
   % Gauss-Seidel
   stype = default('point / line Gauss-Seidel? 1/2 (default is line)',2);
   if stype==2
      sweeps = default('number of Gauss-Seidel sweeps? 1/2/3/4 (default is 2)',2);
   else
      sweeps=1;
   end
else
   % point Jacobi
   sweeps=1;stype=1;
end
npre = default('number of pre-smoothing steps? (default is 1)',1);
npost = default('number of post-smoothing steps? (default is 1)',1);
%
% construct smoother 
if domain==1
   smooth_data = mg_smooth(mgdata,nc,sweeps,smooth,stype);
elseif domain==2
   smooth_data = mg_smooth_ell(mgdata,nc,sweeps,smooth,stype);
else
   smooth_data = mg_smooth_step(mgdata,nc,sweeps,smooth,stype);
end
