function [xp,yp] = mapc2p(xc,yc)
%
% specifies the mapping to curvilinear coordinates -- should be consistent
% with mapc2p.f
%
%

%     xp = xc + (yc+1)/2;
      xp = xc + (abs(yc+.2)+ .8)/2;
      yp = yc;
