% AN APPLICATION OF LIFTING-LINE THEORY (a numerical 
% tool based on Prandtl's lifting-line theory):
%
%      Analysis of EXAMPLE 7.6 
%
% This code was written by Daniel T. Valentine, 2012/2016.
% 
   clear;clc;
%
  rho = 1.2256; % Density of fluid, kg/m^3.
  V = 89.4;     % Free-stream speed, m/s.
%
% Wing geometric characteristics:
  s = 12.192/2;    % Wing semi-span, m.
  b = 2*s;         % Wing span.
  AR = 5.333;      % Aspect ratio.
  S = b^2/AR;      % Plan area.
  cbar = S/b;      % Standard mean chord (SMC).
  N = 100;   % N+1 trailing vortex lines, N intervals 
             % (that is, N segments of the lifting line).
%
% Fourier analysis of circulation distribution check:
NZ = 10;
theta = 0:pi/2/NZ:pi/2;
cc = 3.048*(1 - (3.048-1.524)/3.048.*cos(theta));  
ainff = 5.5*( 1 - (5.5-5.8)/5.5.*cos(theta));
alphaGive = 5.5*(1 - (5.5-3.5)/5.5.*cos(theta));
sinthe = sin(theta);
muu = cc.*ainff/(8*s);
mualphsin = muu.*alphaGive.*sinthe*pi/180;
for mm = 1:NZ+1
     mt = mm;
    for nz = 1:NZ+1
        nn = 2*nz - 1;
CC(mm,nz) = sin(nn*theta(mt))*(sinthe(mt) + nn*muu(mt));
    end
end
for mm = 1:NZ
    for nn=1:NZ
        C(mm,nn) = CC(mm+1,nn);
    end
end
C;
bb = mualphsin(2:end);
Cinv = inv(C);
A = C\bb'
% % REMARK: Coefficients given in example checks out. 
%  Equal segmentation in the spanwise direction:
  y = -s:b/N:s;
% Width of segments of lifting line:
  dy = diff(y);
  yp = y(1:length(y)-1);
  yp = yp + dy/2;   % Center of segment of lifting line.
  co = 3.048;       % Chord distribution coefficient.
  c = co*( 1 - abs(cos(acos(yp/s))/2));% Chord distribution.
  cbarc = sum(c.*dy)/b; 
  c = c.*cbar/cbarc;
% 
% Specification of circulation distribution on lifting line:
  Go = 4*s*V;   % Maximum value of circulation.
%
  dGa(N+1) = 0; % Initialization of trailing vortex strengths.
    ainf = 5.5 *(1 + abs((5.5-5.8)/5.5.*yp/s));
    alphaGiven = 5.5*(1 - abs((5.5-3.5)/5.5.*yp/s));
    plot(yp,alphaGiven,'k'),hold on
    A1 = A(1);
    A3 = A(2);
    A5 = A(3);
    A7 = A(4);
    A9 = A(5);
    A11 = A(6);
    A13 = A(7);
    A15 = A(8);
    A17 = A(9);
    A19 = A(10);
    Ga = Go*(A1*sin(acos(-yp/s)) + A3*sin(3*acos(-yp/s)) ...
         + A5*sin(5*acos(-yp/s)) + A7*sin(7*acos(-yp/s)) ...
         + A9*sin(9*acos(-yp/s)) + A11*sin(11*acos(-yp/s)) ...
         + A13*sin(13*acos(-yp/s)) + A15*sin(15*acos(-yp/s)) ...
         + A17*sin(17*acos(-yp/s)) + A19*sin(19*acos(-yp/s)) );
%
% The strength of the trailing vortex system is the difference in 
% Ga from one segment of the wing or lifting-line to another. 
  dG = diff(Ga);
  dGa(1) = Ga(1); 
  dGa(2:N) = dG(1:N-1);
  dGa(N+1) = -dGa(1);
%
% Calculation of the downwash at the center of each of the wing 
% segments (that is , at each m location) by numerical integration 
% (that is, summation in n) of the downwash induced by the trailing 
% vortex sheet (or system):
  for m = 1:length(yp)
      vp(m) = 0;
      for n = 1:length(y)
        dvpdy(m,n) = dGa(n)/(4*pi*(yp(m)-y(n)));
        vp(m) = vp(m) + dGa(n)/(4*pi*(yp(m)-y(n)));
      end
  end
%
% Calculation of lift and induced drag components of aerodynamic-force
% by spanwise integration:
  L = 0;
  Dv = 0;
 for m=1:length(yp)
     L = L + rho*V*dy(m)*Ga(m);
     Dv = Dv + rho*vp(m)*dy(m)*Ga(m);
 end
  CL = L/(rho*V^2*S/2);
  CDI = Dv/(rho*V^2*S/2);
%
VR = sqrt( V.^2 + vp.^2 );
dFR = rho.*VR.*Ga;
 alpha = (180/pi).*dFR./(ainf.*rho.*VR.^2.*c/2);
% Flow angle
 beta = atan2(vp,V)*180/pi;
%
% Symmetric airfoils
 plot(yp,alpha+beta),xlabel('Lifting-line span direction')
 ylabel('Geometric angle of attack in degrees measured from V')
disp(' Design parameters:')
disp([' rho =  ' num2str(rho) ' kg/m^3'])
disp([' V =    ' num2str(V) ' m/s'])
disp([' AR =   ' num2str(AR)])
disp([' s =    ' num2str(s) ' m'])
disp([' cbar = ' num2str(cbar) ' m'])
disp([' S =    ' num2str(S) ' m^2'])
disp([' L =    ' num2str(L) ' N'])
disp([' Dv =   ' num2str(Dv) ' N'])
disp([' CL =   ' num2str(CL) ])
disp([' CDv =  ' num2str(CDI) ])
disp([' CL/CDv =  ' num2str(CL/CDI) ])
