% Algorithm 5.1. 
% The 3D reconstruction algorithm from two views - calibrated case
% as described in Chapter 5, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
%
% Last modified 5/5/2005

% Following shell generates synthetic views of point features
% under motion, with hypothetical calibration matrix and 
% computes the motion between the views and 3D Euclidean structure of the scene
% Jana Kosecka, George Mason University, 2002
% ==================================================================

close all; clear;

FRAMES = 2;

% cube in the object frame
 XW = [0 1 1 0 0 1 1 0 0.2 0.8 0.2 0.8 ;
       0 0 1 1 0 0 1 1 1.5 1.5 1.5 1.5;
       1 1 1 1 0 0 0 0 0.8 0.8 0.2 0.2 ;
       1 1 1 1 1 1 1 1 1   1   1   1];
NPOINTS = size(XW,2); 

% cube in camera frame
XC = zeros(4,NPOINTS,FRAMES);

% initial displacement between world and first camera
Rinit = rot_matrix([1 1 1],0); 
Zinit = 5;
Tinit = [ Rinit(1,:) 0;
          Rinit(2,:) 0;
          Rinit(3,:) Zinit;
          0 0 0 1];

% coordinate in first camera frame
XC(:,:,1) = Tinit*XW;

% retinal coordinates
xr1 = project(XC(:,:,1));

% intrinsic parameter matrix
A = [600  0  300;
       0 600 300;
       0  0   1];

% pixel coordinates   
xim1 = A*xr1;


       
% camera motion
rot_axis = [0 1 0];
trans = [1,0,1];
theta = -10*pi/180;

R = rot_matrix(rot_axis,theta);
T = [R(1,:) trans(1);
     R(2,:) trans(2);
     R(3,:) trans(3);
     0   0   0     1];
XC(:,:,2) = T*XC(:,:,1);

figure; hold on;
plot3_struct(XC(1,:,1),XC(2,:,1),XC(3,:,1));
plot3(XC(1,:,1),XC(2,:,1), XC(3,:,1),'.'); 
xlabel('x'); ylabel('y'); zlabel('z');
draw_frame_scaled([diag([1,1,1]), zeros(3,1)],0.5);
draw_frame_scaled(T(1:3,:),0.5); text(0,0,0.2,'1'); 
view(20,20); grid on; axis equal;
title('Cameras and 3D structure configuration');

% perspective projection
xr2 = project(XC(:,:,2));
xim2 = A*xr2; 

figure;
subplot(121); hold on;
plot(xim1(1,:),xim1(2,:),'.');
plot_struct(xim1(1,:),xim1(2,:));
grid on; axis equal; axis([0 600 0 600]);
title('image 1');

subplot(122); hold on;
plot(xim2(1,:),xim2(2,:),'.');
plot_struct(xim2(1,:),xim2(2,:));
grid on; axis equal; axis([0 600 0 600]);
title('image 2');

% final result - recovered Rotation and translation up to scale
[Tf, Rf] = essentialDiscrete(xr1, xr2);

[X,lambda] = compute3DStructure(xr1, xr2, Rf, Tf);

figure; hold on;
plot3_struct(X(1,:,1),X(2,:,1),X(3,:,1));
plot3(X(1,:,1),X(2,:,1), X(3,:,1),'.'); 
title('final reconstruction'); view(15, 50); box on; grid on;
