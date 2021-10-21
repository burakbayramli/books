function [xp,yp] = mapc2p(xc,yc)
%
% specifies the mapping to curvilinear coordinates -- should be consistent
% with mapc2p.f
%
%

      xp = xc .* cos(yc);
      yp = xc .* sin(yc);
