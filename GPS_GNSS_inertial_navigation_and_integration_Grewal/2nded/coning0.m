%
% Illustrates coning motion by plotting body-fixed axes in inertial frame
% during coning motion with the 'coning axis' vertical.
%
% Output is a movie (ConingMovie) of the body-fixed axis directions
% in navigation coordinates
%
clear all;
close all;
Omega = 10*2*pi;
Theta = 10*pi/180;
t     = 0:.001:.3;
figure;
ring11 = [];
ring12 = [];
ring13 = [];
ring21 = [];
ring22 = [];
ring23 = [];
ring31 = [];
ring32 = [];
ring33 = [];
az     = 135;
el     = 30;
for k=1:length(t),
    plot3([0,1],[0,0],[0,0],'g-');
    view(az,el);
    %az = az + 1;
    hold on;
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
    if UV(1)>=UV(2)
        VA = 'top';
    else
        VA = 'bottom';
    end;
    text(UV(1),UV(2),UV(3),'RotVec','Color','y','HorizontalAlignment',HA,'VerticalAlignment','VA)
    cT  = cos(Theta);
    sT  = sin(Theta);
    ocT = 1 - cT;
    RM  = [cOt^2*ocT + cT, cOt*sOt*ocT, sOt*sT; cOt*sOt*ocT, sOt^2*ocT + cT, -cOt*sT; -sOt*sT, cOt*sT, cT];
    ring11 = [ring11,RM(1,1)];
    ring12 = [ring12,RM(1,2)];
    ring13 = [ring13,RM(1,3)];
    ring21 = [ring21,RM(2,1)];
    ring22 = [ring22,RM(2,2)];
    ring23 = [ring23,RM(2,3)];
    ring31 = [ring31,RM(3,1)];
    ring32 = [ring32,RM(3,2)];
    ring33 = [ring33,RM(3,3)];
    plot3(ring11,ring21,ring31,'b-');
    plot3(ring12,ring22,ring32,'b-');
    plot3(ring13,ring23,ring33,'b-');
    plot3([0,RM(1,1)],[0,RM(2,1)],[0,RM(3,1)],'r-');
    plot3([0,RM(1,2)],[0,RM(2,2)],[0,RM(3,2)],'r-');
    plot3([0,RM(1,3)],[0,RM(2,3)],[0,RM(3,3)],'r-');
    axis([-1 1 -1 1 -1.2 1.2]);
    text(0,0,1.1,'Coning Axis','Color','g','VerticalAlignment','bottom','HorizontalAlignment','center');
    %axis equal;
    hold off;
    pause(.001);
end;
