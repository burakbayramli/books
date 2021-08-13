function [rhsq,hlsq] = q1res_diff(xy,ev)
%Q1RES_DIFF computes interior residuals for rectangular Q1 grid
%   [rhsq,hlsq] = q1res_diff(xy,ev);
%   input
%          xy        vertex coordinate vector  
%          ev        element mapping matrix
%   output
%          rhsq      elementwise L2 residual norms
%          hlsq      elementwise areas
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);
nel=length(ev(:,1));
fprintf('computing Q1 interior residuals...  ')
%
% set up 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt;  t(1) = -gpt;
      s(2) =  gpt;  t(2) = -gpt;
      s(3) =  gpt;  t(3) =  gpt;
      s(4) = -gpt;  t(4) =  gpt;
%
         rhsq = zeros(nel,1);
         hlsq = zeros(nel,1);
% inner loop over elements    
         for ivtx = 1:4
         xl_v(:,ivtx) = x(ev(:,ivtx));
         yl_v(:,ivtx) = y(ev(:,ivtx)); 
		 end
% loop over 2x2 Gauss points
         for igpt = 1:4
         sigpt=s(igpt);
         tigpt=t(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         rhs = gauss_source(sigpt,tigpt,xl_v,yl_v);
         rhsq(:) = rhsq(:) + rhs(:) .* rhs(:) .* jac(:); 
         hlsq(:) = hlsq(:) + jac(:);
% end of Gauss point loop
         end
%
fprintf('done\n')
return
