function x = mg_iter(As,x0,f,smooth_data,level,npre,npost,sweeps)
%MG_ITER performs one GMG iteration
%   x = mg_iter(As,x0,f,smooth_data,level,npre,npost,sweeps)
%   input
%          As           coefficient matrix
%          x0           initial iterate
%          f            right-hand side
%          smooth_data  structure containing smoothing operators 
%          level        grid level
%          npre         number of presmoothing steps
%          npost        number of postsmoothing steps
%          sweeps       type of smoothing strategy used for Gauss-Seidel
%                       smoothing
%   output
%          x            result of multigrid step
%
%   IFISS function: AR, HCE; 21 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
A = As(level).matrix;
P = As(level).prolong;
%
if level==2,
   x = A\f;
else
   % presmooth 
   x = mg_pre(A,x0,f,npre,smooth_data,level,sweeps);
   % Restrict residual   
   r = f - A*x;
   rc = P'*r;
   % coarse grid correction
   cc = mg_iter(As,zeros(size(rc)),rc,smooth_data,level-1,npre,npost,sweeps);
   % add this line for W-cycle
%  cc = mg_iter(As,cc,rc,smooth_data,level-1,npre,npost,sweeps);
   x = x + P*cc;   
   % postsmooth
   x = mg_post(A,x,f,npost,smooth_data,level,sweeps);
end
