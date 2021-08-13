      function [chi,dchidx,dchidy] = lderiv(s,t,xl,yl)
%LDERIV evaluates derivatives of linear shape functions 
%   [chi,dchidx,dchidy] = lderiv(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          chi       elementwise shape functions
%          dchidx    x derivatives of chi
%          dchidy    y derivatives of chi
%
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
% evaluate linear shape functions
     [chi_e,dchids,dchidt] = lshape(s,t);
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
%
     for ivtx = 1:3
         chi(:,ivtx) = chi_e(ivtx)*one_v;
		 dchidx(:,ivtx) = ( dchids(:,ivtx).*dydt(:) - dchidt(:,ivtx).*dyds(:));
         dchidy(:,ivtx) = (-dchids(:,ivtx).*dxdt(:) + dchidt(:,ivtx).*dxds(:));
      end
      return


