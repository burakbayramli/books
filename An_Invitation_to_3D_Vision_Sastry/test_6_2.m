% given two uncalibrated views and three vanishing points
% 1. compute projective reconstruction
% 2. using projective coordinates of the vanishing points compute 
%    plane at infinity
% 3. compute affine upgrade matrix Ha 
% 4. Upgrade projective structure to an affine one Xa = Ha*Xp

     
% number of points 
close all; clear;

% number of points 
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
Rinit = rot_matrix([1 1 1],pi/8); 
Zinit = 5;
Tinit = [ Rinit(1,:) -0.5 ;
          Rinit(2,:) -0.5 ;
          Rinit(3,:) Zinit;
         0 0 0 1];
XC(:,:,1) = Tinit*XW;

% transformation is expressed wrt to the camera frame
ax = [1 1 0];
trans = [-1,0,1];
angle = 15;
rot_axis = ax/norm(ax);
theta = (angle)*pi/180;
%   represents rotation between 1st and 2nd frame 
R = rot_matrix(rot_axis,theta)
T = [ R(1,:) trans(1);
      R(2,:) trans(2);
      R(3,:) trans(3);
      0   0   0     1         ]
XC(:,:,2) = T*XC(:,:,1);

% intrinsic parameter matrix
A = [600  0  300;
       0 600 300;
       0  0   1];

% perspective projection
xr1 = project(XC(:,:,1));
xim1 = A*xr1;
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

% affine reconstruction using vanishing points
% 1. compute projective reconstruction 
% 2. compute projective coordinates of plane at infinity
% 3. upgrade to affine reconstruction via Ha

% plot in the image coordinate frame
subplot(121);  hold on;
% compute and plot vanishing points 
x1 = xim1(1:2,:); % remove third homog. coordinate
vp1(:,1) = vanishing_point([x1(:,1), x1(:,5)], [x1(:,2), x1(:,6)]);
vp1(:,2) = vanishing_point([x1(:,1), x1(:,2)], [x1(:,5), x1(:,6)]);
vp1(:,3) = vanishing_point([x1(:,2), x1(:,3)], [x1(:,6), x1(:,7)]);
x2 = xim2(1:2,:);
subplot(122);
vp2(:,1) = vanishing_point([x2(:,1), x2(:,5)], [x2(:,2), x2(:,6)]);
vp2(:,2) = vanishing_point([x2(:,1), x2(:,2)], [x2(:,5), x2(:,6)]);
vp2(:,3) = vanishing_point([x2(:,2), x2(:,3)], [x2(:,6), x2(:,7)]);
drawnow;

% estimate fundametal matrix
F = dfundamental(xim1, xim2);

% epipole computation
[U, S, V] = svd(F');
ep = V(:,3)/V(3,3)

% fundamental matrix canonical decomposition and computation of
% projections matrices with respect to 1st and 2nd frame
M = skew(ep)'*F;
P1 = [diag([1 1 1]) zeros(3,1)];
P2 = [M ep];

% compute projective structure
[XP,lambda] = compute3DStructure(xim1, xim2, M, ep);

% compute projective coordinates of the three vanishing points
for i=1:3
   [VP,lambda] = compute3DStructure(vp1(:,i), vp2(:,i), P2(1: 3,1:3), ...
				     P2(:,4));
   VXP(:,i) = VP(:,:,1);
   pause
   
end

% compute the plane at infinity
A = [VXP(1:3,1)';
     VXP(1:3,2)';
     VXP(1:3,3)'];
% this is the plane at infinity
v = inv(A)*[-1 -1 -1]'

% this is the projective transformation for updating projective
% structure to an affine one
Ha = [diag([1,1,1]), zeros(3,1);
        v'          , 1];

for i = 1:NPOINTS
    XA(:,i,1) = Ha*XP(:,i,1);
    XA(:,i,1) = XA(:,i,1)/XA(4,i,1);
end

figure;
plot3_struct(XA(1,:,1),XA(2,:,1),XA(3,:,1));
view(200,20); grid on; axis on; box on;
title('affine reconstruction');


