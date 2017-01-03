%
% Generate illustration of coning motion
%
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
clear all;
close all;
figure;
axis equal;
axis off;
Az = 135;
sA = sin(Az*pi/180);
cA = cos(Az*pi/180);
El = 30;
sE = sin(El*pi/180);
cE = cos(El*pi/180);
view(Az,El);
alpha = .3;
uv = [sA*cE;,-cA*cE;sE]; % unit vector to camera & illumination source
theta = 15*pi/180;
framenumber = 0;
for OmegatDeg=0:2:360,
    framenumber = framenumber + 1;
    Omegat = OmegatDeg*pi/180;
    RotVec = [theta*cos(Omegat);theta*sin(Omegat);0];
    CTMat  = RotVec2CTMat(RotVec);
    close all;
    figure;
    plot3(1.2,0,0,'w.');
    hold on;
    plot3(0,1.2,0,'w.');
    plot3(0,0,1.2,'w.');
    axis equal;
    axis off;
    view(Az,El);
    result = faintarrow([0;0;0],CTMat(:,1),uv,alpha);
    result = faintarrow([0;0;0],CTMat(:,2),uv,alpha);
    result = faintarrow([0;0;0],CTMat(:,3),uv,alpha);
    result = notsofatarrow([0;0;0],[1;0;0],uv);
    result = notsofatarrow([0;0;0],[0;1;0],uv);
    result = notsofatarrow([0;0;0],[0;0;1],uv);
    M(framenumber) = getframe;
end;
movie(M);
% savefile = 'ConingMovie.mat';
% save(savefile,'M');
% movie2avi(M,'coning.avi','compression','None');