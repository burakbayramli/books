% Solution to Problem 6 in Chapter 3
% The psi equations below are identical
clear;clc
xx = -.8+.001:.002:.8;
yy = -1+.001:.002:0;
%yy = +.05:.1:2;
m = 1; V = 1; c = 1;
for nx = 1:length(xx)
    for ny = 1:length(yy)
        x(ny,nx) = xx(nx);
        y(ny,nx) = yy(ny);
%         ps2(ny,nx) = V*y(ny,nx) - (m/2/pi)* ...
%             atan2(2*c*y(ny,nx),(x(ny,nx)^2+y(ny,nx)^2-c^2));
%         ps1(ny,nx) = V*y(ny,nx) + (m/2/pi)*atan2(y(ny,nx),x(ny,nx)+c) ...
%             - (m/2/pi)*atan2(y(ny,nx),x(ny,nx)-c);
%         ps1(ny,nx) = (m/2/pi)*atan2(y(ny,nx),x(ny,nx)+c) ...
%             - (m/2/pi)*atan2(y(ny,nx),x(ny,nx)-c);
        ps1(ny,nx) = (m/2/pi)*atan2(y(ny,nx)+2*c*cos(30*pi/180),x(ny,nx)) ...
            + (m/2/pi)*atan2(y(ny,nx),x(ny,nx)+c) ...
            + (m/2/pi)*atan2(y(ny,nx),x(ny,nx)-c);
    end
end
contour(x,y,ps1,50),hold on, contour(x,y,ps1,[-.25],'k')
% cL = min(min(ps1))/2;
% cH = max(max(ps1))/2;
% contour(x,y,ps1,[cL cH],'k'),axis image
% contour(x,y,ps1)
% hold on
% contour(x,y,ps2,'m')