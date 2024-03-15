% Horseshoe vortex
%
clear;clc
L = 250*1000; % N of lift
b = 34;       % m span
s = b/2;
V = 40;       % m/s forward speed 
rho = 1.2256;   % kg/m/m/m
X = 18.3;     % m behind center of wing
H = 6.1;      % m above ground
% Assume elliptic loading
sprime = s * pi/4; % Half distance between trailing vortices.
Go = L/(rho*V*2*sprime);
 beta = atan2(sprime,X);
 wp = (Go/(2*pi*sprime))*(1+sec(beta));
% tanbeta = sprime/L; 
% Induction by lifting line
x1 = 0; y1 = -sprime; z1 = 0; % End 1 of vortex element
x2 = 0; y2 =  sprime; z2 = 0; % End 2 of vortex element
x3 = X; y3 =  0; z3 = H; % Field point on center line
c2 = (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2; c = sqrt(c2);
a2 = (x3-x2)^2 + (y3-y2)^2 + (z3-z2)^2; a = sqrt(a2);
b2 = (x3-x1)^2 + (y3-y1)^2 + (z3-z1)^2; b = sqrt(b2);
cosA = (b2 + c2 - a2)/(2*b*c);
cosB = (a2 + c2 - b2)/(2*a*c);
h = sqrt(x3^2 + (z3 - z2)^2);
ui = (Go/4/pi/h)*(cosA+cosB);
tanth = 2*z3/x3; th = atan(tanth); costh = cos(th);
w = ui*costh;
% Induction by trailer 1
% x1 = 0; y1 = -1; z1 = -1; % End 1 of trailer 1 vortex
% x2 = 0; y2 =  1; z2 = -1; % End 1 of trailer 2 vortex 
% x3 = 2; y3 =  0; z3 =  1; % Field point
a312 = (y3 - y1)^2 + (z3-z1)^2; a31 = sqrt(a312);
b312 = b2; b31 = sqrt(b312);
c312 = (x3 - x1)^2; c31 = sqrt(c312);
if c31 <= 10*eps
    cosAt1 = 0;
else
    cosAt1 = (b312 + c312 - a312)/(2*b31*c31);
end
ht1 = a31;
uit1 = (Go/4/pi/ht1)*(cosAt1+1);
costht1 = -y1/ht1;
wt1 = uit1*costht1;
% Induction by trailer 2
% x1 = 0; y1 = -1; z1 = -1; % End 1 of trailer 1 vortex
% x2 = 0; y2 =  1; z2 = -1; % End 1 of trailer 2 vortex 
% x3 = 2; y3 =  0; z3 =  1; % Field point
a322 = (y3 - y2)^2 + (z3-z2)^2; a32 = sqrt(a322);
b322 = a2; b32 = sqrt(b322);
c322 = (x3 - x2)^2; c32 = sqrt(c322);
if c32 <= 10*eps
    cosAt2 = 0;
else
    cosAt2 = (b322 + c322 - a322)/(2*b32*c32);
end
ht2 = a32;
uit2 = (Go/4/pi/ht2)*(cosAt2+1);
costht2 = y2/ht2;
wt2 = uit2*costht2;
wtotali = wt1+wt2+w;
ang = atan2( wp - wtotali,V)*180/pi
% THIS RESULT CHECKS WITH THE FORMULA IN THE EXAMPLE ON TAIL-PLANE