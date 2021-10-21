function [xp,yp] = mapc2p(xc,yc)
%
% specifies the mapping to curvilinear coordinates -- should be consistent
% with mapc2p.f
%
%
      r0 = 1;
      r1 = 5;

      yc1 = min(yc, abs(pi/2-yc));
      yc1 = min(yc1, abs(pi-yc));
      yc1 = min(yc1, abs(3*pi/2-yc));
      yc1 = min(yc1, abs(2*pi-yc));

      r = r0 + (r1*sqrt(1 + tan(yc1).^2) - r0) .* xc;

      xp = r .* cos(yc);
      yp = r .* sin(yc);
