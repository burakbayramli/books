function [cpsi,dcpsidx,dcpsidy] = cderiv(s,t,xl,yl)
%CDERIV evaluates derivatives of bicubic shape functions 
%   [psi,dpsidx,dpsidy] = cderiv(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          cpsi       elementwise shape functions
%          dcpsidx    x derivatives of cpsi
%          dcpsidy    y derivatives of cpsi
%
%   IFISS function: QL; 29 Apr 2010.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
% evaluate bicubic shape functions
     [cpsi_e,dcpsids,dcpsidt] = cshape(s,t);
% local derivatives
      dxds = zero_v;
      dxdt = zero_v;
      dyds = zero_v;
      dydt = zero_v;
	   jac = zero_v;
    invjac = zero_v; 
%
      for ivtx = 1:4
         dxds(:) = dxds(:) + xl(:,ivtx) .* one_v*dphids(ivtx);
         dxdt(:) = dxdt(:) + xl(:,ivtx) .* one_v*dphidt(ivtx);
         dyds(:) = dyds(:) + yl(:,ivtx) .* one_v*dphids(ivtx);
         dydt(:) = dydt(:) + yl(:,ivtx) .* one_v*dphidt(ivtx);
      end
%
      jac(:) = dxds(:).*dydt(:) - dxdt(:).*dyds(:);
%
% check element Jacobian
      if any(jac < 1e-9)
      fprintf('Bad element warning ...\n')
         if any(jac <= 0.0)
         error('singular Jacobian ... Aborted ...')
         end
      end
%      invjac(:) = one_v ./ jac(:);
%
     for ivtx = 1:16
         cpsi(:,ivtx) = cpsi_e(ivtx)*one_v;
		 dcpsidx(:,ivtx) = ( dcpsids(:,ivtx).*dydt(:) - dcpsidt(:,ivtx).*dyds(:));
         dcpsidy(:,ivtx) = (-dcpsids(:,ivtx).*dxdt(:) + dcpsidt(:,ivtx).*dxds(:));
      end
      return
