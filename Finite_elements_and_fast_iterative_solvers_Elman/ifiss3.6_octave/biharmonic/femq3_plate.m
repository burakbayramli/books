function [a,r,f] = femq3_plate(xy,ev)
%FEMQ3_PLATE bicubic coefficient matrix generator
%   [A,Q,f] = femq3_plate(xy,ev);
%   input
%          xy         nodal coordinate vector  
%          ev         element mapping matrix
%   output
%          A          stiffness matrix
%          Q          mass matrix 
%          f          rhs vector
%
%   Natural boundary conditions apply. Essential conditions
%   must be explicitly enforced by calling function clampedbc.
%   IFISS function: DJS; 14 August 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
debug=0;
x=xy(:,1); y=xy(:,2);
nvtx=length(x); ndof=4*nvtx;
nel=length(ev(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));
fprintf('setting up Q3x biharmonic matrices...  ')
%
% initialise global matrices
      a = sparse(ndof,ndof);
      r = sparse(ndof,ndof);
      f = zeros(ndof,1);
%
% construct the integration rule
ngpt=4; nngpt=ngpt*ngpt;
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx));
		end
        ae = zeros(nel,16,16);
        re = zeros(nel,16,16);
        fe = zeros(nel,16);
% loop over Gauss points
        for igpt = 1:nngpt
        sigpt=s(igpt);
        tigpt=t(igpt);
        wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         rhs = gauss_load(sigpt,tigpt,xl_v,yl_v);
         [psi,dpsidx,dpsidy] = bfsderiv(sigpt,tigpt,xl_v,yl_v);
         [dpsi2dx2,dpsi2dy2] = bfsderiv_2(sigpt,tigpt,xl_v,yl_v);
            for j = 1:16
               for i = 1:16
               ae(:,i,j)  = ae(:,i,j)  + wght*(dpsi2dx2(:,i) + dpsi2dy2(:,i)) ...
                                            .*(dpsi2dx2(:,j) + dpsi2dy2(:,j)).*jac(:);
               re(:,i,j)  = re(:,i,j)  + wght*psi(:,i).*psi(:,j).*jac(:);
               end
            end
% load vector
            for j = 1:16
            fe(:,j) = fe(:,j)  +  wght* rhs(:) .* psi(:,j) .* jac(:);
            end
% end of Gauss point loop
         end

% debug
if debug
fprintf('\n second element stiffness matrix\n')
ae2=squeeze(ae(2,:,:));
fprintf('columns 1 through 8: \n')
disp([ae2(:,1:8)]),
fprintf('columns 9 through 16:\n')
disp([ae2(:,9:16)]),
fprintf('second elment load vector: \n')
fe2=fe(2,:)';
disp([fe2]),
fprintf('\n second element mass matrix\n')
re2=squeeze(re(2,:,:));
fprintf('columns 1 through 8: \n')
disp([re2(:,1:8)]),
fprintf('columns 9 through 16:\n')
disp([re2(:,9:16)]),
save('testmatrices','re2','ae2','fe2')
end

% create mapping vector
      mv=[ev,ev+nvtx,ev+2*nvtx,ev+3*nvtx];
%
% perform assembly of global matrix  and source vector
      for krow=1:16
      nrow= mv(:,krow);
          for kcol=1:16
		  ncol= mv(:,kcol);
          a = a + sparse(nrow,ncol,ae(:,krow,kcol),ndof,ndof);
          r = r + sparse(nrow,ncol,re(:,krow,kcol),ndof,ndof);
          end
      f(nrow,1) = f(nrow,1) + fe(:,krow);
      end
%
%
fprintf('done\n')
return

