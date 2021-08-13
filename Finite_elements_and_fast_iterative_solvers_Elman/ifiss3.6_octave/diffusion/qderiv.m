      function [psi,dpsidx,dpsidy] = qderiv(s,t,xl,yl)
%QDERIV evaluates derivatives of biquadratic shape functions 
%   [psi,dpsidx,dpsidy] = qderiv(s,t,xl,yl);
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
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
% evaluate biquadratic shape functions
     [psi_e,dpsids,dpsidt] = qshape(s,t);
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
      invjac(:) = one_v ./ jac(:);
%
     for ivtx = 1:9
         psi(:,ivtx) = psi_e(ivtx)*one_v;
		 dpsidx(:,ivtx) = ( dpsids(:,ivtx).*dydt(:) - dpsidt(:,ivtx).*dyds(:));
         dpsidy(:,ivtx) = (-dpsids(:,ivtx).*dxdt(:) + dpsidt(:,ivtx).*dxds(:));
      end
      return
