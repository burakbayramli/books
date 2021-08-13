function [qua,dquadx,dquady] = qqderiv(s,t,xl,yl)
%QQDERIV derivatives of biquartic shape functions 
%   [quar,dquardx,dquardy] = qqderiv(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          qua       elementwise shape functions
%          dquadx    x derivatives of qar
%          dquady    y derivatives of qar
%
%   IFISS function: DJS; 3 January 2011.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
% evaluate biquadratic shape functions
     [qua_e,dquads,dquadt] = qqshape(s,t);
% local derivatives
      dxds = zero_v;
      dxdt = zero_v;
      dyds = zero_v;
      dydt = zero_v;
%	   jac = zero_v;
%    invjac = zero_v; 
%
      for ivtx = 1:4
         dxds(:) = dxds(:) + xl(:,ivtx) .* one_v*dphids(ivtx);
         dxdt(:) = dxdt(:) + xl(:,ivtx) .* one_v*dphidt(ivtx);
         dyds(:) = dyds(:) + yl(:,ivtx) .* one_v*dphids(ivtx);
         dydt(:) = dydt(:) + yl(:,ivtx) .* one_v*dphidt(ivtx);
      end
%
%      jac(:) = dxds(:).*dydt(:) - dxdt(:).*dyds(:);
%
% check element Jacobian
%      if any(jac < 1e-9)
%      fprintf('Bad element warning ...\n')
%         if any(jac <= 0.0)
%         error('singular Jacobian ... Aborted ...')
%         end
%      end
%      invjac(:) = one_v ./ jac(:);
%
     for ivtx = 1:25
         qua(:,ivtx) = qua_e(ivtx)*one_v;
		 dquadx(:,ivtx) = ( dquads(:,ivtx).*dydt(:) - dquadt(:,ivtx).*dyds(:));
         dquady(:,ivtx) = (-dquads(:,ivtx).*dxdt(:) + dquadt(:,ivtx).*dxds(:));
      end
      return
