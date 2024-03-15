%
% Elemental lifting-line theory: See Section 7.5.3 because 
% the application in this code is for elliptic loading. Hence, 
% the numerical-lifting-line theory gives the correct downwash
% over most of the wing span. The value is the same as determined 
% from Equation (7.32).
%
% Code written by Daniel T. Valentine, 2012/2016.
   clear;clc;
%
% Wing geometric characteristics:
  AR = 1;
  s = 1; 
  S = s^2/AR;
  c = 1;
%
Go = 1;
%
  rho = 1; VInf = 1; % Free-stream condition.
%
% N = 200;
% theta = 0:pi/N:pi;
% y = -(s/2)*cos(theta);
% Segmentation of the spanwise direction:
  N = 100;
  y = -s/2:s/N:s/2; % N+1 lines, N intervals (or segments of lifting line) 
                    % (y ==> location of trailing vortices)
  dy = diff(y);
  dGa(N+1) = 0; % Initialization of trailing vortex strengths.
%
% Since cos(theta) = -y*2/s, we can determine Ga for 
% an elliptic loading in yp, the center of each element of lifting line, 
% as follows:
    yp = y(1:length(y)-1); % Center of segment of lifting line.
    yp = yp + dy/2;
    Ga = sin(acos(-yp*2/s));
%
% The strength of the trailing vortex system is the difference in 
% in Ga from one segment of the wing, liftin-line model to another. 
  dG = diff(Ga);
  dGa(1) = Ga(1); 
  dGa(2:N) = dG(1:N-1);
  dGa(N+1) = -dGa(1);
%
% Calculation of the downwash at the center of ean of the wing segments:
for m = 1:length(yp)
    vp(m) = 0;
    for n = 1:length(y)
        vp(m) = vp(m) + dGa(n)/(4*pi*(yp(m)-y(n)));
%        pause
    end
end
%
plot(yp,vp,'o'), title('Downwash at the center of each wing segment')
xlabel(' Spanwise coordinate, y'),ylabel(' Downwash, -wp ')
%
% Lift and induced drag components of aerodynamic-force vector
%
CL = 0;
CDI = 0;
for m=1:length(yp)
    CL = CL + rho*VInf*dy(m)*Ga(m);
    CDI = CDI + rho*vp(m)*dy(m)*Ga(m);
end
CL = CL/S
CDI = CDI/S
% NOTE: This code, as written, gives CL = pi/4 and CDI = pi/8 to four 
% decimal places. In edition, vp = 0.5 and it is constant. Thus, this 
% relatively crude numerical method gives accurrate results. 
