%
% Evaluate elements of vertical state transition matrix versus t
%
clear all;
close all;
REarth     = 0.6371009e7;  % Mean Earth radius [m]
GMEarth    = 0.3986004e15; % Gravity constant m^3/s^2]
tauU       = sqrt(REarth^3/(2*GMEarth)); 
hours      = 10;
minutes    = 60*hours;
seconds    = 60*minutes;
k = 0;
for t=0:60:seconds;
    k       = k + 1;
    time(k) = t;
    rat     = t/tauU;
    p11(k)  = cosh(rat);
    p12(k)  = tauU*sinh(rat);
    p21(k)  = sinh(rat)/tauU;
end;
semilogy(time/3600,p11,'k-',time/3600,p12,'k-.',time/3600,p21,'k:','LineWidth',1.5);
xlabel('TIME IN HOURS','FontWeight','bold','FontSize',14);
ylabel('\Phi(t)','FontWeight','bold','FontSize',14);
gtext('\Phi_{11} = \Phi_{22}','FontWeight','bold','FontSize',14);
gtext('\Phi_{12}','FontWeight','bold','FontSize',14);
gtext('\Phi_{21}','FontWeight','bold','FontSize',14);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
