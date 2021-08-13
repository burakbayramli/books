      function [psi,dpsidx,dpsidy] = bfsderiv(s,t,xl,yl)
%BFSDERIV evaluates derivatives of BFS shape functions
%   [psi,dpsidx,dpsidy] = bfsderiv(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          psi       elementwise shape functions
%          dpsidx    x derivatives of psi
%          dpsidy    y derivatives of psi
%
%   IFISS function: DJS; 2 February 2019.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
      nel=length(xl(:,1));
      zero_v = zeros(nel,1); one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
% evaluate BFS shape functions on reference element
     [psi_e,dpsids,dpsidt,dpsi2ds2,dpsi2dt2,dpsi2dsdt] = bfsshape(s,t);
% local derivatives
      dxds = zero_v;
      dxdt = zero_v;
      dyds = zero_v;
      dydt = zero_v;
%
      for ivtx = 1:4
         dxds(:,1) = dxds(:,1) + xl(:,ivtx) .* one_v*dphids(ivtx);
         dxdt(:,1) = dxdt(:,1) + xl(:,ivtx) .* one_v*dphidt(ivtx);
         dyds(:,1) = dyds(:,1) + yl(:,ivtx) .* one_v*dphids(ivtx);
         dydt(:,1) = dydt(:,1) + yl(:,ivtx) .* one_v*dphidt(ivtx);
      end
%
      jac = dxds(:).*dydt(:) - dxdt(:).*dyds(:);
   invjac = one_v ./ jac;
       hx = dxds; hy = dydt;
%
% check for any nonrectangle element(s)
      if (max(abs(dxdt)) > 10*eps) | (max(abs(dyds)) >= 10*eps)
      error('Oops.. nonrectangular element  ... bfsderiv aborted ...')
      end
%
%  rescale to give Hermite functions defined on the physical element
       for ivtx = 1:4
       psi(:,ivtx) = psi_e(ivtx)*one_v;
       dpsidx(:,ivtx) = dpsids(:,ivtx).*dydt(:).*invjac(:)./hx(:);
       dpsidy(:,ivtx) = dpsidt(:,ivtx).*dxds(:).*invjac(:)./hy(:);
       end
%  rescale to give Hermite functions defined on the physical element
       for ivtx = 5:8
       psi(:,ivtx) = psi_e(ivtx)*one_v.*hx(:);
       dpsidx(:,ivtx) = dpsids(:,ivtx).*dydt(:).*invjac(:).*hx(:);
       dpsidy(:,ivtx) = dpsidt(:,ivtx).*dxds(:).*invjac(:).*hx(:);
       end
       for ivtx = 9:12
       psi(:,ivtx) = psi_e(ivtx)*one_v.*hy(:);
       dpsidx(:,ivtx) = dpsids(:,ivtx).*dydt(:).*invjac(:).*hy(:);
       dpsidy(:,ivtx) = dpsidt(:,ivtx).*dxds(:).*invjac(:).*hy(:);
       end
       for ivtx = 13:16
       psi(:,ivtx) = psi_e(ivtx)*one_v.*hx(:).*hy(:);
       dpsidx(:,ivtx) = dpsids(:,ivtx).*dydt(:).*invjac(:).*hx(:).*hy(:);
       dpsidy(:,ivtx) = dpsidt(:,ivtx).*dxds(:).*invjac(:).*hx(:).*hy(:);
       end
       return
