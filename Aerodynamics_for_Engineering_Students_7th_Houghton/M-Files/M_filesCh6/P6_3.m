clear;clc
% Problem 3: NACA 8210
m = 0.08; p = 0.2; t = 0.1; alpha = 0;
thetap = acos(1 - 2*p);
Ao = (m/pi/p/p)*((2*p-1)*thetap+sin(thetap)) ...
+ (m/(pi*(1-p)^2))*( (2*p-1)*(pi-thetap)-sin(thetap) );
A1 = (2*m/pi/p/p)*( (2*p-1)*sin(thetap) + sin(2*thetap)/4 ...
+ thetap/2) - (2*m/(pi*(1-p)^2))*( (2*p-1)*sin(thetap) ...
+ sin(2*thetap)/4 - (pi - thetap)/2 );
A2 = (2*m/pi/p/p)*( (2*p-1)*(sin(2*thetap)/4+thetap/2) ...
+ sin(thetap) - sin(thetap)^3/3 ) - (2*m/(pi*(1-p)^2))* ...
( (2*p-1)*(sin(2*thetap)/4-(pi-thetap)/2) + sin(thetap) ...
- sin(thetap)^3/3 );
CL = pi*(A1-2*Ao) + 2*pi*alpha;
CMqc = - pi*(A1-A2)/4;
disp(' Ao A1 A2 CL CMqc ')
disp([Ao A1 A2 CL CMqc])
% RESULTS: The CL and the CMqc are the same as in the text.
% Ao A1 A2 CL CMqc
% 0.0704 0.3920 0.1723 0.7890 -0.1726
%
% % Example 4.2 on page 195 of text:
% m = 0.04; p = 0.4; t = 0.12; alpha = 0;
% thetap = acos(1 - 2*p);
% Ao = (m/pi/p/p)*((2*p-1)*thetap+sin(thetap)) ...
% + (m/(pi*(1-p)^2))*( (2*p-1)*(pi-thetap)-sin(thetap) );
% A1 = (2*m/pi/p/p)*( (2*p-1)*sin(thetap) + sin(2*thetap)/4 ...
% + thetap/2) - (2*m/(pi*(1-p)^2))*( (2*p-1)*sin(thetap) ...
% + sin(2*thetap)/4 - (pi - thetap)/2 );
% A2 = (2*m/pi/p/p)*( (2*p-1)*(sin(2*thetap)/4+thetap/2) ...
% + sin(thetap) - sin(thetap)^3/3 ) - (2*m/(pi*(1-p)^2))* ...
% ( (2*p-1)*(sin(2*thetap)/4-(pi-thetap)/2) + sin(thetap) ...
% - sin(thetap)^3/3 );
% CL = pi*(A1-2*Ao) + 2*pi*alpha;
% CMqc = - pi*(A1-A2)/4;
% disp(' Ao A1 A2 CL CMqc ')
% disp([Ao A1 A2 CL CMqc])
% % RESULTS: The following agree with what is in the text.
% % Ao A1 A2 CL CMqc
% % 0.0090 0.1630 0.0228 0.4556 -0.1101