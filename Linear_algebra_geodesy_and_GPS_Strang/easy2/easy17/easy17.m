% EASY17  Drawing GPS constellation in inertial and ECEF frames.
%         The third plot illustrates computation of sub-satellite points
%
%         To creat only once the internal format of the
%         almanac we proceed through the following steps:
%                1. Download almanc (brdc1550.08n) from
%                   http://www.ngs.noaa.gov/CORS/Data.thml
%                2. Call rinexe('brdc1550.08n','alm.dat');
%                3. Run this file easy17

%Kai Borre 16-07-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/07/16  $

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

eph = get_eph('alm.dat');
[b,i,j] = unique(eph(1,:),'first'); % i contains the unique columns of eph
n = length(i); % total number of SVs
sv = eph(1,i); % sv contains SV numbers

%%%%%%%%%%%%% inertial version  %%%%%%%%%%%%%%%%%%
figure(1);
hold on
p = zeros(48,3);
for t = 1:48
    p(t,:) = satposin(eph(21,1)+900*t, eph(:,i(1)))';
end
orbit = line('color',[0.15+.05*i(1) .25+.03*i(1) .09],...
    'linestyle','-','linewidth',.5,...
    'erase','xor','xdata',[],'ydata',[],'zdata',[]);
set(orbit,'xdata',p(:,1)','ydata',p(:,2)','zdata',p(:,3)');
drawnow
plot3(p(1,1),p(1,2),p(1,3),'w*')
%text(p(1,1),p(1,2),p(1,3),num2str(sv(1))) % adds SV numbers

for k = 2:n
    for t = 1:48
        p(t,:) = satposin(eph(21,1)+900*t, eph(:,i(k)))';
    end
    orbit = line('color',[0.15+.025*sv(k) .25+.015*sv(k) .09],...
        'linestyle','-','linewidth',.5,...
        'erasemode','xor','xdata',[],'ydata',[],'zdata',[]);
    set(orbit,'xdata',p(:,1)','ydata',p(:,2)','zdata',p(:,3)');
    drawnow
    plot3(p(1,1),p(1,2),p(1,3),'w*')
    %text(p(1,1),p(1,2),p(1,3),num2str(sv(k)))
end

[x,y,z] = sphere(50);
load topo;
props.AmbientStrength = 0.1;
props.DiffuseStrength = 1;
props.SpecularColorReflectance = .3; %.5;
props.SpecularExponent = 20;
props.SpecularStrength = 1;
props.FaceColor= 'texture';
props.EdgeColor = 'none';
props.FaceLighting = 'phong';
props.Cdata = topo;
surface(6700000*x,6700000*y,6700000*z,props);
light('position',[-1 0 1]); % -1 0 1
light('position',[1.5 -0.5 -0.5], 'color', [.6 .2 .2]); % -1.5 .5 -.5
view([-90 0]) % az = -90 and el = 0
axis equal off
hold off
print -depsc2 easy171
%print -deps2 easy171

%%%%%%%%%%%%%% ECEF version %%%%%%%%%%%%%%%%%%%
figure(2);
hold on
for t = 1:48
    p(t,:) = satpos(eph(21,1)+900*t, eph(:,i(1)))';
end
orbit = line('color',[0.15+.05*i(1) .25+.03*i(1) .09],...
    'linestyle','-','linewidth',.5,...
    'erase','xor','xdata',[],'ydata',[],'zdata',[]);
set(orbit,'xdata',p(:,1)','ydata',p(:,2)','zdata',p(:,3)');
drawnow
plot3(p(1,1),p(1,2),p(1,3),'w*')
%text(p(1,1),p(1,2),p(1,3),num2str(sv(1))) % adds SV numbers

for k = 2:n
    for t = 1:48
        p(t,:) = satpos(eph(21,1)+900*t, eph(:,i(k)))';
    end
    orbit = line('color',[0.15+.025*sv(k) .25+.015*sv(k) .09],...
        'linestyle','-','linewidth',.5,...
        'erasemode','xor','xdata',[],'ydata',[],'zdata',[]);
    set(orbit,'xdata',p(:,1)','ydata',p(:,2)','zdata',p(:,3)');
    drawnow
    plot3(p(1,1),p(1,2),p(1,3),'w*')
    %text(p(1,1),p(1,2),p(1,3),num2str(sv(k)))
end

[x,y,z] = sphere(50);
load topo;
props.AmbientStrength = 0.1;
props.DiffuseStrength = 1;
props.SpecularColorReflectance = .3; %.5;
props.SpecularExponent = 20;
props.SpecularStrength = 1;
props.FaceColor= 'texture';
props.EdgeColor = 'none';
props.FaceLighting = 'phong';
props.Cdata = topo;
surface(6700000*x,6700000*y,6700000*z,props);
light('position',[1 0 1]); % -1 0 1
light('position',[1.5 -0.5 -0.5], 'color', [.6 .2 .2]); % -1.5 .5 -.5
view([180 0]) % az and el found from experiments
axis equal off
hold off
print -depsc2 easy172
%print -deps2 easy172

%%%%%%%%%%%%%%%% plot of sub-satllites points for satellite k  %%%%%%%%%%

% The sub-sattliuet points are computed as teh inersection between a sphere
% of radius 6,700 km and the line from origin to the stallite. 
% A more refined computation will substitute the sphere with an ellipsoid. 
% The corresponding computations are more evolved. Many applications most 
% likely only need the present accuracy 

figure(3);
hold on
for k = 2 %:n
    proj= zeros(48,3);
    for t = 1:48
        p(t,:) = satpos(eph(21,1)+900*t, eph(:,i(k)))';
        proj(t,:) = 6700000*p(t,:)/norm(p(t,:));
    end
    plot3(proj(:,1),proj(:,2),proj(:,3),'linewidth',2,'color','r')
end
[x,y,z] = sphere(50);
load topo;
props.AmbientStrength = 0.1;
props.DiffuseStrength = 1;
props.SpecularColorReflectance = .3; %.5;
props.SpecularExponent = 20;
props.SpecularStrength = 1;
props.FaceColor= 'texture';
props.EdgeColor = 'none';
props.FaceLighting = 'phong';
props.Cdata = topo;
surface(6700000*x,6700000*y,6700000*z,props);
light('position',[1 0 1]); % -1 0 1
light('position',[1.5 -0.5 -0.5], 'color', [.6 .2 .2]); % -1.5 .5 -.5
view([150 0]) % az and el found from experiments
axis equal off
hold off
print -depsc2 easy173
%print -deps2 easy173
%%%%%%%%%%%%% end easy17.m %%%%%%%%%%%%%%%%%%%%%%%


