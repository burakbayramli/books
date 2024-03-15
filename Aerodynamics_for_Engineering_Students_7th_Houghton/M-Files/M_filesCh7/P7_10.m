% P 5.10: The script below applies to points along the 
% centerline perpendicular to the span. 
clear;clc
%
L = 250*1000; % N of lift
b = 34;       % m span
s = b/2;
V = 40;       % m/s forward speed 
rho = 1.23;   % kg/m/m/m
X = 18.3;     % m behind center of wing
H = 6.1;      % m above ground
% Assume elliptic loading
sprime = s * pi/4; % Half distance between trailing vortices.
% tanbeta = sprime/L; 
beta = atan2(sprime,X);
Go = L/(rho*V*2*sprime);
wp = (Go/(2*pi*sprime))*(1+sec(beta))/V;
wp = wp*V;
% ang = atan2(wp,V)*180/pi
% The change in downwash due to image
% IMAGE
% Biot-Savart Law of an element of a line vortex
Go = -Go; 
% Induction by lifting line
x1 = 0; y1 = -sprime; z1 = -H; % End 1 of vortex element
x2 = 0; y2 =  sprime; z2 = -H; % End 2 of vortex element
x3 = X; y3 =  0; z3 = H;       % Field point on center line
c2 = (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2; c = sqrt(c2);
a2 = (x3-x2)^2 + (y3-y2)^2 + (z3-z2)^2; a = sqrt(a2);
b2 = (x3-x1)^2 + (y3-y1)^2 + (z3-z1)^2; b = sqrt(b2);
cosA = (b2 + c2 - a2)/(2*b*c);
cosB = (a2 + c2 - b2)/(2*a*c);
h = sqrt(x3^2 + (z3 - z2)^2);
ui = (Go/4/pi/h)*(cosA+cosB);
costh = x3/h;
w = ui*costh;
% Induction by trailers
ht1 = sqrt(sprime^2+(z3-z1)^2);
cosAt1 = x3/sqrt(ht1^2+x3^2);
uit1 = (Go/4/pi/ht1)*(cosAt1+1);
costht1 = sprime/ht1;
wt1 = 2*uit1*costht1;
%
 wtotali = wt1+w;
 ang = atan2(wp + wtotali,V)*180/pi
