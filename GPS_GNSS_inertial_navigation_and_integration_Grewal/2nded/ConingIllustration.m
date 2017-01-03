%
% Illustrates coning motion by plotting body-fixed axes in inertial frame
% during coning motion with the 'coning axis' vertical, then generates
% separate plots of the "figure-8" trajectories of the body-fixed 
% pitch and roll axes in earth-fixed coordinates.
%
% from Global Positioning Systems, Inertial Navigation and Integration,
% Second Edition, by M. S. Grewal, L. R. Weill and A. P. Andrews,
% published by Wiley & Sons, 2005
% 
% © 2005
%
clear all;
close all;
Omega = 10*2*pi;   % Coning rate   = 10 Hz
Theta = 10*pi/180; % Coning angle  = 10 deg
t     = 0:.001:.3; % Sampling rate = 1000/sec
%                    Three cycles simulated
figure;
AT11 = [];       % These arrays used for 
AT12 = [];       % plotting body-fixed axis trajectories
AT13 = [];
AT21 = [];
AT22 = [];
AT23 = [];
AT31 = [];
AT32 = [];
AT33 = [];
az     = 135;
el     = 30;
for k=1:length(t),
    plot3([0,1],[0,0],[0,0],'g-');
    title('Coning Illustration');
    text(0,0,2.1,'Grewal, Weill & Andrews','HorizontalAlignment','center','VerticalAlignment','middle');
    text(0,0,1.8,'Global Positioning Systems, Inertial Navigation, and Integration','HorizontalAlignment','center','VerticalAlignment','middle');
    text(0,0,1.5,'2nd Ed., ©Wiley, 2005','HorizontalAlignment','center','VerticalAlignment','middle');
    view(az,el);
    %az = az + 1;
    hold on;
    text(0,0,-1,['t = ',num2str(t(k)),' sec'],'HorizontalAlignment','center');
    plot3([0,0],[0,1],[0,0],'g-');
    plot3([0,0],[0,0],[0,1],'g-');
    sOt = sin(Omega*t(k));
    cOt = cos(Omega*t(k));
    UV  = [cOt; sOt; 0;];
    plot3([0,UV(1)],[0,UV(2)],[0,0],'y-');
    if UV(1)>=UV(2)
        HA = 'right';
    else
        HA = 'left';
    end;
    %
    if UV(1)>=-UV(2)
        VA = 'top';
    else
        VA = 'bottom';
    end;
    text(UV(1),UV(2),UV(3),'Rotation','Color','y','HorizontalAlignment',HA,'VerticalAlignment',VA);
    cT  = cos(Theta);
    sT  = sin(Theta);
    ocT = 1 - cT;
    %
    % Angular rates in body-fixed coordinates
    %
    AR  = [-sin(Omega*t(k))*Omega*sin(Theta);cos(Omega*t(k))*Omega*sin(Theta);-Omega*(-1+cos(Theta))];
    PitchRate(k) = AR(1);
    RollRate(k)  = AR(2);
    YawRate(k)   = -AR(3);
    %
    % Coordinate transformation from body-fixed to navigation coordinates
    %
    RM  = [cOt^2*ocT + cT, cOt*sOt*ocT, sOt*sT; cOt*sOt*ocT, sOt^2*ocT + cT, -cOt*sT; -sOt*sT, cOt*sT, cT];
    %
    % Angular rates in navigation coordinates
    %
    AR  = RM*AR;
    EastRate(k)  = AR(1);
    NorthRate(k) = AR(2);
    UpRate(k)    = AR(3);
    uAR = AR/norm(AR);
    plot3([UV(1),UV(1)+uAR(1)],[UV(2),UV(2)+uAR(2)],[UV(3),UV(3)+uAR(3)],'m:');
    plot3( [0,uAR(1)],[0,uAR(2)],[0,uAR(3)],'m-');
    if uAR(1)>=uAR(2)
        HA = 'right';
    else
        HA = 'left';
    end;
    %
    if uAR(1)>=-uAR(2)
        VA = 'top';
    else
        VA = 'bottom';
    end;
    %text(UV(1)+uAR(1),UV(2)+uAR(2),UV(3)+uAR(3),'Rate','Color','m','HorizontalAlignment',HA,'VerticalAlignment',VA);
    text(uAR(1),uAR(2),uAR(3),'Rate','Color','m','HorizontalAlignment',HA,'VerticalAlignment',VA);
    %
    % Trajectories of body-fixed unit vectors in navigation coordinates
    %
    AT11 = [AT11,RM(1,1)];
    AT12 = [AT12,RM(1,2)];
    AT13 = [AT13,RM(1,3)];
    AT21 = [AT21,RM(2,1)];
    AT22 = [AT22,RM(2,2)];
    AT23 = [AT23,RM(2,3)];
    AT31 = [AT31,RM(3,1)];
    AT32 = [AT32,RM(3,2)];
    AT33 = [AT33,RM(3,3)];
    %
    % Plot trajectories of Pitch-Roll-minusYaw unit vectors in nav coord
    %
    plot3(AT11,AT21,AT31,'b-');
    plot3(AT12,AT22,AT32,'b-');
    plot3(AT13,AT23,AT33,'b-');
    %
    % Plot East-North-Up coordinate axes in red
    %
    plot3([0,RM(1,1)],[0,RM(2,1)],[0,RM(3,1)],'r-');
    plot3([0,RM(1,2)],[0,RM(2,2)],[0,RM(3,2)],'r-');
    plot3([0,RM(1,3)],[0,RM(2,3)],[0,RM(3,3)],'r-');
    text(RM(1,1),RM(2,1),RM(3,1),'Pitch','Color','r','HorizontalAlignment','right','VerticalAlignment','bottom');
    text(RM(1,2),RM(2,2),RM(3,2),'Roll','Color','r','HorizontalAlignment','left','VerticalAlignment','bottom');
    axis([-1.5 1.5 -1.5 1.5 -1.2 1.2]);
    text(0,0,1.1,'Coning Axis','Color','g','VerticalAlignment','bottom','HorizontalAlignment','center');
    axis off;
    hold off;
    pause(.01);
end;
figure;
subplot(2,2,1),
plot(AT21*180/pi,AT31*180/pi,'k-');
title('Pitch Axis Trajectory');
axis equal;
xlabel('North Component [deg]');
ylabel('Vertical Component [deg]');
subplot(2,2,2),
plot(AT12*180/pi,AT32*180/pi,'k-');
title('Roll Axis Trajectory');
axis equal;
xlabel('East Component [deg]');
ylabel('Vertical Component [deg]');
subplot(2,2,3),
plot(AT13*180/pi,AT23*180/pi,'k-');
title('-Yaw Axis Trajectory');
axis equal;
xlabel('North Component [deg]');
ylabel('East Component [deg]');
subplot(2,2,4),
axis([-1 1 -1 1]);
axis off;
text(0,.7,'Coning Illustration','HorizontalAlignment','center','VerticalAlignment','middle');
text(0,.4,'Grewal, Weill & Andrews','HorizontalAlignment','center','VerticalAlignment','middle');
text(0,.1,'GPS, INS & Integration','HorizontalAlignment','center','VerticalAlignment','middle');
text(0,-.2,'2nd Edition','HorizontalAlignment','center','VerticalAlignment','middle');
text(0,-.7,'©Wiley, 2005','HorizontalAlignment','center','VerticalAlignment','middle');
figure;
subplot(3,1,1),
plot(t,PitchRate,'k-');
title('Angular Rates in Body-Fixed Coordinates');
xlabel('time [sec]');
ylabel('Pitch Rate [rad/sec]');
subplot(3,1,2),
plot(t,RollRate,'k-');
xlabel('time [sec]');
ylabel('Roll Rate [rad/sec]');
subplot(3,1,3),
plot(t,YawRate,'k-');
xlabel('time [sec]');
ylabel('Yaw Rate [rad/sec]');

