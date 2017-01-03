% Algorithm 11.6. 
% The projective reconstruction algorithm from two views
% as described in Chapter 6 and 11, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
%
% Last modified 5/5/2005

% Following shell generates synthetic views of point features
% under motion, with hypothetical calibration matrix and 
% computes projective structure of the scene
% Jana Kosecka, George Mason University, 2002
% ==================================================================

% given two uncalibrated views and three vanishing points
% 1. compute fundamental matrix
% 2. compute projective reconstruction


close all; clear;

FRAMES = 2;
PLOTS  = FRAMES + 1;

% cube in the object frame
 XW = [0 1 1 0 0 1 1 0 0.2 0.8 0.2 0.8 ;
       0 0 1 1 0 0 1 1 1.5 1.5 1.5 1.5;
       1 1 1 1 0 0 0 0 0.8 0.8 0.2 0.2 ;
       1 1 1 1 1 1 1 1 1   1   1   1];
NPOINTS = size(XW,2); 

XC = zeros(4,NPOINTS,FRAMES);

% initial displacement between the world and first camera
Rinit = rot_matrix([1 1 1],0); 
Zinit = 5;
Tinit = [ Rinit(1,:) 0 ;
          Rinit(2,:) 0 ;
          Rinit(3,:) Zinit;
         0 0 0 1];
XC(:,:,1) = Tinit*XW;

xr1 = project(XC(:,:,1));

% intrinsic parameter matrix
A = [600  0  300;
       0 600 300;
       0  0   1];

xim1 = A*xr1;

% transformation is expressed wrt to the camera frame
ax = [0 1 0];
trans = [1,0,1];
angle = -20;
rot_axis = ax/norm(ax);
theta = (angle)*pi/180;
%   represents rotation between 1st and 2nd frame 
R = rot_matrix(rot_axis,theta)
T = [ R(1,:) trans(1);
      R(2,:) trans(2);
      R(3,:) trans(3);
      0   0   0     1         ]
XC(:,:,2) = T*XC(:,:,1);

% perspective projection
xr2 = project(XC(:,:,2));
xim2 = A*xr2;

figure; hold on;
plot3_struct(XC(1,:,1),XC(2,:,1),XC(3,:,1));
plot3(XC(1,:,1),XC(2,:,1), XC(3,:,1),'.'); 
xlabel('x'); ylabel('y'); zlabel('z');
draw_frame_scaled([diag([1,1,1]), zeros(3,1)],0.5);
draw_frame_scaled(T(1:3,:),0.5); text(0,0,0.2,'1'); 
view(20,20); grid on; axis equal;
title('Cameras and 3D structure configuration');

figure;
subplot(121); hold on;
plot(xim1(1,:),xim1(2,:),'.');
plot_struct(xim1(1,:),xim1(2,:));
grid on; axis equal; axis([0 600 0 600]);
title('image 1');


subplot(122); hold on;
plot(xim2(1,:),xim2(2,:),'.');
plot_struct(xim2(1,:), xim2(2,:));
grid on; axis equal; axis([0 600 0 600]);
title('image 2');

% estimate fundametal matrix
F = dfundamental(xim1, xim2);

% epipole computation
[U, S, V] = svd(F');
ep = V(:,3)/V(3,3)

% an example of the epipolar line computation
el = F*xim1(:,1);

% fundamental matrix factorization and computation of
% projections matrices with respect to 1st and 2nd frame
M = skew(ep)'*F;
P1 = [diag([1 1 1]) zeros(3,1)];
P2 = [M ep];

[vec,val] = eig(M);

% compute projective structure
[XP,lambda] = compute3DStructure(xim1, xim2, M, ep);

 % plot projective reconstruction
figure; hold on;
plot3(XP(1,:,1),XP(2,:,1),XP(3,:,1),'.');
plot3_struct(XP(1,:,1),XP(2,:,1),XP(3,:,1));
xlabel('x'); ylabel('y'); zlabel('z'); view(220,20);
title('projective reconstruction'); box on; grid on;





