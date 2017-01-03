% Algorithm 5.2. 
% Homography H estimation between two views of a plane
% decompose H into rotation and translation and plane normal
% Described in Chapter 5, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
%
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

% Following shell generates synthetic views of coplanar point features
% under motion, with hypothetical calibration matrix and 
% computes camera motion between the views
% Jana Kosecka, George Mason University, 2002
% ============================================================


close all; clear;
% generate points in the plane
NPOINTS = 50;
XW = [rand(1,NPOINTS); 
      rand(1,NPOINTS);
      zeros(1,NPOINTS); 
      ones(1,NPOINTS)];
XC = zeros(4,NPOINTS,1);

Zinit = 10;
% initial displacement between world and first camera
Rinit = rot_matrix([1 1 0]/norm([1 1 0]),pi/7); 
Tinit = [ Rinit(1,:) 2;
          Rinit(2,:) 3;
          Rinit(3,:) Zinit;
          0 0 0 1];
XC(:,:,1) = Tinit*XW;

% compute the plane normal and the distance given coordinates XC(:,:,1)
P = [];
for i = 1:NPOINTS
    dd = [XC(1,i,1) XC(2,i,1) XC(3,i,1) 1];
    P = [P; dd];
end

[u,s,v] = svd(P);
pn = v(1:3,4);
pd = v(4,4);

% get another image 
t_axis = [1 0 1];
trans = 5*t_axis';
R = rot_matrix([0 1 0], -30*pi/180);
% translation represents origin of the camera frame in the world frame 
T = [ R trans; 0 0 0 1];
XC(:,:,2) = T*XC(:,:,1);  

figure(1); hold on;
plot3(XC(1,:,1), XC(2,:,1), XC(3,:,1), '*r'); box on;
draw_frame_scaled([diag([1 1 1]), ones(3,1)], 2); 
draw_frame_scaled(T,2); axis equal;
view(-20, -15);

% retinal coordinates
xr1 = [XC(1,:,1)./XC(3,:,1); XC(2,:,1)./XC(3,:,1); ones(1, NPOINTS)];
xr2 = [XC(1,:,2)./XC(3,:,2); XC(2,:,2)./XC(3,:,2); ones(1, NPOINTS)];

% calibration matrix
A = [1200 0  600;
      0  900   450;
      0  0     1];

% image coordinates
xim1 = A*xr1;
xim2 = A*xr2;

figure(2);
subplot(121); plot(xim1(1,:), xim1(2,:), '*r'); hold on; 
title('first image');
subplot(122); plot(xim2(1,:), xim2(2,:), '*b'); hold on; 
title('second image');

[Sol,H] = homography2Motion(xr1, xr2)
