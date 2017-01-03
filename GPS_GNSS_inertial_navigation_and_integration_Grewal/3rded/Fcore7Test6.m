%
% Grewal, Andrews, and Bartone,
% Global Navigation Satellite Systems, Inertial Navigation, and Integration
% 3rd Edition, Wiley, 2012
%
% SCHULER-CORIOIS TEST: INITIAL EAST POS ERR OF 100 m
%
clear all;
close all;
Lat = 40;           %  TEST LATITUDE (DEG)
vel = zeros(2,1);   %  INS STATIONARY
Dt = 10;
Fcore = Fcore7(Lat,vel);
Phi   = expm(Dt*Fcore);
t       = 0;
xi      = zeros(7,1);
xi(1)   = 100;
xi0 = xi;
E       = xi(1);
N       = xi(2);
vE      = xi(3);
vN      = xi(4);
tE      = xi(5);
tN      = xi(6);
he      = xi(7);
hours   = 30;
disp('Fcore7 INS ERROR MODEL')
disp([num2str(hours),'-HOUR SIMULATED TEST']);
disp('INS STATIONARY');
disp(['INITIAL ',num2str(xi0(1)),' m EAST POSITION ERROR']);
minutes = hours*60;
seconds = minutes*60;
for k=1:Dt:seconds,
    xi   = Phi*xi;
    t    = [t,k];
    E    = [E,xi(1)];
    N    = [N,xi(2)];
    vE   = [vE,xi(3)];
    vN   = [vN,xi(4)];
    tE   = [tE,xi(5)];
    tN   = [tN,xi(6)];
    he   = [he,xi(7)];
end;
subplot(2,2,1),plot(E,N,'k-','LineWidth',1.5);
axis equal;
xlabel('EASTING [m]','FontWeight','bold','FontSize',12),
ylabel('NORTHING [m]','FontWeight','bold','FontSize',12);
title([num2str(xi0(1)),' m E POS'],'FontWeight','bold','FontSize',12);
subplot(2,2,2),plot(vE,vN,'k-','LineWidth',1.5);
axis equal;
xlabel('E. VEL. [m/s]','FontWeight','bold','FontSize',12),
ylabel('N. VEL. [m/s]','FontWeight','bold','FontSize',12);
title([num2str(Lat),'^o LAT'],'FontWeight','bold','FontSize',12);
subplot(2,2,3),plot(tE*1e6,tN*1e6,'k-','LineWidth',1.5);
axis equal;
xlabel('E. TILT [\mu rad]','FontWeight','bold','FontSize',12),
ylabel('N. TILT [\mu rad]','FontWeight','bold','FontSize',12);
title('Fcore7 MODEL','FontWeight','bold','FontSize',12);
subplot(2,2,4),plot(t/3600,he*1000,'k-',t/3600,N,'k--','LineWidth',1.5);
xlabel('TIME [hr]','FontWeight','bold','FontSize',12),
ylabel('YAW ERR. [mRAD]','FontWeight','bold','FontSize',12);
title([num2str(hours),'-HOUR TEST'],'FontWeight','bold','FontSize',12);

