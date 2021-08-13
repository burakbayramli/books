%MG_CD GMG preconditioner for scalar CD problem
%   IFISS scriptfile: DJS, HCE; 27 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nc=log2(length(x)-1);
%
% compute new MG data or reload existing data?
compute_mg = default('compute / load MG data? 1/2 (default 1)',1);
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
   for level = nc-1:-1:2;
      mgdata(level).matrix=mg_cd_setup(xn,yn,viscosity,outbc);
      mgdata(level).prolong=mg_prolong(2^level,2^level,xn,yn);
      xn=xn(1:2:end);yn=yn(1:2:end);
   end
   fprintf('done\n')
   gohome, cd datafiles, save mgdata_cd.mat mgdata
end
%
% MG parameters
smooth = default('Jacobi / Gauss-Seidel / ILU smoother? 1/2/3 (default is Gauss-Seidel)',2);
if smooth==3 % point ILU
   sweeps=1;stype=1;
elseif smooth==2 % Gauss-Seidel
   stype = default('point / line Gauss-Seidel? 1/2 (default is line)',2);
   if stype==2
      sweeps = default('number of Gauss-Seidel directions? 1/2/3/4 (default is 2)',2);
   else
      sweeps=1;
   end
else % point Jacobi
   sweeps=1;stype=1;
end
npre = default('number of pre-smoothing steps? (default is 1)',1);
npost = default('number of post-smoothing steps? (default is 1)',1);
%
% construct smoother 
smooth_data = mg_smooth(mgdata,nc,sweeps,smooth,stype);
