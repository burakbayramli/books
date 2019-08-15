function test04
% some auxiliary calculations for the Taylor-Hood element
clc, format short, format compact
syms x y
f = [(1-x-y)*(1-2*x - 2*y), x*(2*x-1),y*(2*y-1),4*x*(1-x-y),4*x*y,4*y*(1-x-y)];
g = [1-x-y,x,y];
%  AA = sum(f); AA = simplify(AA)
dgx = [-1,1,0]; dgy = [-1,0,1];

%dfx = diff(f,x)
%dfy = diff(f,y)

dfx = [-3+4*x+4*y, 4*x-1,   0, 4-8*x-4*y, 4*y, -4*y]; %df/dx
dfy = [-3+4*x+4*y,   0, 4*y-1,      -4*x, 4*x,  4-4*x-8*y]; % df/dy

dfx = dfx.'; CC1 = dfx*g; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); C1 = 6*CC3;
dfy = dfy.'; CC1 = dfy*g; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); C2 = 6*CC3;

ff = f.';
M1 = ff*f; M2 = int(M1,x,0,1-y); M3 = int(M2,y,0,1); M = M3*360;

xf = x*f; xf = xf.';
PP1 = xf*f; PP2 = int(PP1,x,0,1-y); PP3 = int(PP2,y,0,1);
P = PP3*7*360;

yf = y*f; yf = yf.';
QQ1 = yf*f; QQ2 = int(QQ1,x,0,1-y); QQ3 = int(QQ2,y,0,1);
Q = QQ3*7*360;

%CC1 = ff*dfx; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); D1 = 30*CC3;
%CC1 = ff*dfy; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); D2 = 30*CC3;

% ---Psi*p_x und Psi*g_y 
CC1 = ff*dgx; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); D1 = CC3;
CC1 = ff*dgy; CC2 = int(CC1,x,0,1-y); CC3 = int(CC2,y,0,1); D2 = CC3;
