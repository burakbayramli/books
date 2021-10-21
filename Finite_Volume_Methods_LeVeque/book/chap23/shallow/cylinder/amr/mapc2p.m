function [xp,yp] = mapc2p(xc,yc)
%
% specifies the mapping to curvilinear coordinates -- should be consistent
% with mapc2p.f
%
%

      xp = xc .* cos(yc);
      yp = xc .* sin(yc);

      %r = r0 + (r1*sqrt(1 + sin(2*yc).^2) - r0) .* xc;
      %xp = r .* cos(yc);
      %yp = r .* sin(yc);
