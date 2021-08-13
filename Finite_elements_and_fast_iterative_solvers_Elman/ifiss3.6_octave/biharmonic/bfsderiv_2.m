function [dpsi2dx2,dpsi2dy2] = bfsderiv_2(s,t,xl,yl)
%BFSDERIV_2 second derivatives of biquadratic shape functions
%   [dpsidx2,dpsidy2] = bfsderiv_2(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%   output
%          dpsi2dx2    x second derivatives of psi
%          dpsi2dy2    y second derivatives of psi
%
%   IFISS function: DJS; 2 February 2019.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
debug=0;
      nel=length(xl(:,1));
      zero_v = zeros(nel,1);
      one_v = ones(nel,1);
%
% evaluate bilinear shape functions 
     [phi_e,dphids,dphidt] = shape(s,t);
     [jac,invjac,phi,dphidx,dphidy] = deriv(s,t,xl,yl);
% evaluate BFS shape functions on the reference element
     [psi,dpsids,dpsidt,dpsi2ds2,dpsi2dt2,dpsi2dsdt] = bfsshape(s,t);

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
     for ivtx = 1:16
     zdpsi2dx2(:,ivtx) = ( dpsi2ds2(:,ivtx).*dydt(:).^2 + dpsi2dt2(:,ivtx).*dyds(:).^2).*invjac.^2;
     zdpsi2dy2(:,ivtx) = ( dpsi2ds2(:,ivtx).*dxdt(:).^2 + dpsi2dt2(:,ivtx).*dxds(:).^2).*invjac.^2;
     end
%    local mesh sizes .. check for rectangles is done is bfsderiv
     hx = dxds; hy = dydt;
%
     for ivtx = 1:4
     dpsi2dx2(:,ivtx) = dpsi2ds2(:,ivtx).*dydt(:).^2.*invjac.^2;
     dpsi2dy2(:,ivtx) = dpsi2dt2(:,ivtx).*dxds(:).^2.*invjac.^2;
     end
%  rescale to give Hermite functions defined on the physical element
    for ivtx = 5:8
    dpsi2dx2(:,ivtx) = dpsi2ds2(:,ivtx).*dydt(:).^2.*invjac.^2.*hx(:);
    dpsi2dy2(:,ivtx) = dpsi2dt2(:,ivtx).*dxds(:).^2.*invjac.^2.*hx(:);
    end
    for ivtx = 9:12
    dpsi2dx2(:,ivtx) = dpsi2ds2(:,ivtx).*dydt(:).^2.*invjac.^2.*hy(:);
    dpsi2dy2(:,ivtx) = dpsi2dt2(:,ivtx).*dxds(:).^2.*invjac.^2.*hy(:);
    end
    for ivtx = 13:16
    dpsi2dx2(:,ivtx) = dpsi2ds2(:,ivtx).*dydt(:).^2.*invjac.^2.*hy(:).*hx(:);
    dpsi2dy2(:,ivtx) = dpsi2dt2(:,ivtx).*dxds(:).^2.*invjac.^2.*hx(:).*hy(:);
    end

%------ debug output
if debug
olddx2=zdpsi2dx2(1,:); newdx2=dpsi2dx2(1,:);
olddy2=zdpsi2dy2(1,:); newdy2=dpsi2dy2(1,:);
fprintf('\n  xx  and yy derivative checks \n')
edx2=olddx2./newdx2; edy2=olddy2./newdy2;
disp([edx2',edy2']),
end
%------ end debug output

   return
