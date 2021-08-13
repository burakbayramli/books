function [jac,invjac,phi,dphidx,dphidy] = deriv(s,t,xl,yl)
%DERIV evaluates derivatives of bilinear shape functions 
%   [jac,invjac,phi,dphidx,dphidy] = deriv(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          jac       elementwise jacobian (evaluated at (s,t))
%          invjac    elementwise inverse of jacobian
%          phi       elementwise shape functions
%          dphidx    x derivatives of phi
%          dphidy    y derivatives of phi
%
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
%
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
      invjac(:) = one_v ./ jac(:);
%
      for ivtx = 1:4
         phi(:,ivtx) = phi_e(ivtx)*one_v;
		 dphidx(:,ivtx) = ( dphids(:,ivtx).*dydt(:) - dphidt(:,ivtx).*dyds(:));
         dphidy(:,ivtx) = (-dphids(:,ivtx).*dxdt(:) + dphidt(:,ivtx).*dxds(:));
      end
      return
