function [xcent,zcent,rcent,ycent] = fusev(fuse_s)
% Tornado function (internal): extract centerline and radius info from fuselage struct fuse_s
% Input
%   fuse_s    CEASIOM aircraft fuselage struct
%
% Output
%   xcent     x-coord of points on fuselage centercurve
%   zcent     z-coord ...
%   rcent     fuselage radius (vertical)
%   ycent     d:o             (horizontal)
%
% calls
%   --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%
%----------------------------------------------------
% In:
% CEASIOM fuselage struct fuse_s
% Centercurve assumed symmetric around y = 0
% Out:
% centerpolyline vertices              :  xcent,zcent
% Vert. cross section radii at vertices:  rcent
% Hor. cross section radii at vertices :  ycent
%----------------------------------------------------
dv(1) = 0;
dv(2) = fuse_s.Forefuse_X_sect_vertical_diameter;
dv(3) = fuse_s.Aftfuse_X_sect_vertical_diameter;
dv(4) = dv(3);
dv(5) = 0;

dh    = dv;
dh(2) = fuse_s.Forefuse_X_sect_horizontal_diameter;
dh(3) = fuse_s.Aftfuse_X_sect_horizontal_diameter;
dh(4) = dh(3);

Ln = dv(2)*fuse_s.epsilon_nose;
Lt = dv(3)*fuse_s.epsilon_tail;
L  = fuse_s.Total_fuselage_length;
Lc = L - Ln - Lt;
%
% Centerline shape
x(1) = 0;
x(2) = x(1) + Ln;
x(3) = x(2) + fuse_s.fraction_fore*Lc;
x(4) = x(2) + Lc;
x(5) = x(4) + Lt;
%
z(3) = 0;
z(4) = 0;
z(2) = z(3) + fuse_s.shift_fore*dv(2);
z(1) = z(2) - Ln*tan(fuse_s.phi_nose*pi/180);
z(5) = z(4) + Lt*tan(fuse_s.phi_tail*pi/180);
%
% nose and tail shapes
betn = 0.54-0.1*tan((fuse_s.omega_nose-fuse_s.phi_nose)*pi/180);
bett = 0.54-0.1*tan((fuse_s.omega_tail-fuse_s.phi_tail)*pi/180);
n    = 3;
wn   = linspace(0,1,n).^2;
xn   = x(1)+(x(2)-x(1))*wn;
zn   = z(1)+(z(2)-z(1))*wn;
Rn   = dv(2)*wn.^betn/2;
Yn   = dh(2)*wn.^betn/2;

wt   = 1 - fliplr(wn);
xt   = x(4) + (x(5)-x(4))*wt;
zt   = z(4) + (z(5)-z(4))*wt;
Rt   = dv(4)*(1-wt).^bett/2;
Yt   = dh(4)*(1-wt).^bett/2;
xcent = [xn,x(3),xt];
zcent = [zn,z(3),zt];
rcent = [Rn,dv(3)/2,Rt];
ycent = [Yn,dh(3)/2,Yt];
end