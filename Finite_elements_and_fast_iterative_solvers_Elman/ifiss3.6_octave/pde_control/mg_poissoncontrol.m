%MG_POISSONCONTROL sets up inputs for geometric multigrid solver
%IFISS scriptfile: JWP, AR, HCE, DJS; 4 July 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

nc=log2(length(y)-1);
%
% compute new MG data
% h=2^(1-nc);
fprintf('Setting up MG data ...')
% top level
mgdata(nc).matrix=A;
if domain==1 % square
  mgdata(nc).prolong=mg_prolong(2^nc,2^nc,x,y);
elseif domain==2 % L-shaped
  x=x';y=y';
  mgdata(nc).prolong=mg_prolong_ell(nc,x,y);
end
xn=x;yn=y;
% loop over remaining levels
for level = nc-1:-1:2;
  xn=xn(1:2:end);yn=yn(1:2:end);
  if domain==1 % square
     mgdata(level).matrix=mg_diff_setup(xn,yn);
     mgdata(level).prolong=mg_prolong(2^level,2^level,xn,yn);
  elseif domain==2 % L-shaped
     mgdata(level).matrix=mg_diff_setup_ell(xn,yn);
     mgdata(level).prolong=mg_prolong_ell(level,xn,yn);
  end
end
fprintf('done\n')
% gohome, cd datafiles, save mgdata_diff.mat mgdata
if domain~=1, x=x';y=y';, end % square domain
%
% MG parameters
smooth = default('Jacobi / ILU smoother? 1/3 (default is Jacobi)',1);
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
npre = default('number of pre- and post-smoothing steps? (default is 2)',2);
npost = npre;
%
% construct smoother 
if domain==1
   smooth_data = mg_smooth(mgdata,nc,sweeps,smooth,stype);
elseif domain==2
   smooth_data = mg_smooth_ell(mgdata,nc,sweeps,smooth,stype);
end

