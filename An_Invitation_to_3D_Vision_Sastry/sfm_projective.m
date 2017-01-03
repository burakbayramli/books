% Algorithm 11.6. 
% The projective reconstruction algorithm from two views
% as described in Chapter 5, "An introduction to 3-D Vision"
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

 

close all; clear;
affine = 0;

NPOINTS = 20;
FRAMES = 2;
PLOTS  = FRAMES + 1;
% transformation is expressed wrt to the camera frame
actual_trans(1,:) = [0 1 -1];
ax = [1 1 1];
rot_axis = ax/norm(ax);
theta = 0*pi/180;
fov = 60;

% image size in focal lengths units
im_size = 2*tan(fov*pi/180/2);

% image in pixels
im_pixels = 60;
Z_min = 10;			% minimum depth in focal lengths
Z_max = 15;			% maximum depth in focal lengths
Zinit = Z_max-(Z_max-Z_min)/2;
Zinit = 5;

% cube in the object frame
 XW = [0 1 1 0 0 1 1 0 0.2 0.8 0.2 0.8 ;
       0 0 1 1 0 0 1 1 1.5 1.5 1.5 1.5;
       1 1 1 1 0 0 0 0 0.8 0.8 0.2 0.2 ;
       1 1 1 1 1 1 1 1 1   1   1   1];

 XWX = [ 2  4  2  4  2  4  2  4;
         1  1  1  1  0  0  0  0;
         0  0  2  2  0  0  2  2;
         1  1  1  1  1  1  1  1];

% XW = [XW, XWX];

NPOINTS = 12; 


figure
plot3(XW(1,1:8,1),XW(3,1:8,1),XW(2,1:8,1),'*r');
hold on
plot3(XW(1,9:12,1),XW(3,9:12,1),XW(2,9:12,1),'*b');
plot3_struct(XW(1,:,1),XW(3,:,1),XW(2,:,1));
xlabel('x'); ylabel('z'); zlabel('y');
view(220,20);
grid off; 
axis off;

pause;


XC = zeros(4,NPOINTS,FRAMES);

% initial displacement
Rinit = rot_matrix([1 1 1],0); 
if affine 
Rinit = rot_matrix([1 1 1],pi/8); 
end

Tinit = [ Rinit(1,:) -0.5 ;
          Rinit(2,:) -0.5 ;
          Rinit(3,:) Zinit;
         0 0 0 1];
XC(:,:,1) = Tinit*XW;

subplot(1,PLOTS,1);
plot3_struct(XC(1,:),XC(3,:),-XC(2,:));
hold on;
plot3(XC(1,:),XC(3,:), -XC(2,:),'.');
xlabel('x'); ylabel('z'); zlabel('y');
view(20,20);
grid on
axis equal;

XC(2,:,1) = -XC(2,:,1);
x1 = XC(1,:,1)./XC(3,:,1);
y1 = XC(2,:,1)./XC(3,:,1);

im_scale = im_pixels/im_size;
f = 3;
% intrinsic parameter matrix
A = [im_scale*f  0      im_pixels/2;
       0     im_scale*f im_pixels/2;
       0       0           1          ]

% A = diag([1,1,1]);
% pick a vector v and generate points on the plane v^T.Ax = -1;
NPLANE_POINTS = 20;
v = [1 1 1];
[q1,s,q2] = svd(v);
coeff = randn(2,NPLANE_POINTS);
% shortest solution
qbar = pinv(v)*-1; 
% shortest solution + anyting which is in null space of v
% generates points on the plane
for i = 1:NPLANE_POINTS
q_plane(:,i) = qbar + coeff(1,i)*q2(:,2) + coeff(2,i)*q2(:,3);
end

frame1_n = [x1; y1; ones(1,NPOINTS)];

frame1_im = A*frame1_n;
x1_im = frame1_im(1,:);
y1_im = frame1_im(2,:);
%x1_im  = im_pixels/2 +  x1 * im_scale;
%y1_im  = im_pixels/2 +  y1 * im_scale;

subplot(1,PLOTS,2);
hold on;
plot(x1,y1,'.');
plot_struct(x1,y1);
grid on;
axis equal; 
% axis([-0.2 0.2 -0.2 0.2]);
pause

if affine 
   subplot(1,PLOTS,3);   % plot in the image coordinate frame
   hold on;
   plot(x1_im,y1_im,'.');
   plot_struct(x1_im,y1_im);
   grid on;
   axis equal;
   axis([0 im_pixels 0 im_pixels])
   points = [x1_im;y1_im];
   % plot vanishing points 
   van_point1(:,1) = vanishing_point([points(:,1), points(:,5)], ...
                                      [points(:,2), points(:,6)]);
   van_point1(:,2) = vanishing_point([points(:,1), points(:,2)], ...
                                       [points(:,5), points(:,6)]);
   van_point1(:,3) = vanishing_point([points(:,2), points(:,3)], ...
                                      [points(:,6), points(:,7)]);
   drawnow;
end


% transformation is expressed wrt to the camera frame
ax = [0 1 0];
rot_axis = ax/norm(ax);
theta = 0*pi/180;
actual_trans = [1,0,1];
angle = 20;
im_size = 4;
theta = (angle)*pi/180;
%   represents rotation about world frame 
R0 = rot_matrix(rot_axis,theta)
%   translation represents origin of the camera frame
%   in the world frame 
R = R0';     % transpose
trans = R*actual_trans'
     T = [R(1,:) trans(1);
          R(2,:) trans(2);
          R(3,:) trans(3);
          0   0   0     1         ]
XC(:,:,2) = T*XC(:,:,1);

% perspective projection
x1 = XC(1,:,1)./XC(3,:,1);
y1 = XC(2,:,1)./XC(3,:,1);
x2 = XC(1,:,2)./XC(3,:,2);
y2 = XC(2,:,2)./XC(3,:,2);
frame2_n = [x2; y2; ones(1,NPOINTS)];
frame2_im = A*frame2_n;
x2_im = frame2_im(1,:);
y2_im = frame2_im(2,:);

subplot(1,PLOTS,2);
hold on;
plot(x2,y2,'.');
plot_struct(x2,y2);
grid on;
axis equal;

 subplot(1,PLOTS,3);
 hold on;
 plot(x2_im,y2_im,'.');
 plot_struct(x2_im,y2_im);
 points = [x2_im;y2_im];

if affine
   van_point2(:,1) = vanishing_point([points(:,1), points(:,5)], ...
                                          [points(:,2), points(:,6)]);
  van_point2(:,2) = vanishing_point([points(:,1), points(:,2)], ...
                                          [points(:,5), points(:,6)]);
  van_point2(:,3) = vanishing_point([points(:,2), points(:,3)], ...
                                          [points(:,6), points(:,7)]);
end

F = dfundamental([x1_im;y1_im;ones(1,NPOINTS)], [x2_im;y2_im;ones(1,NPOINTS)])

% projective reconstruction 
% epipole computation, F factorization
[U, S, V] = svd(F);
p = V(:,3)/V(3,3)
plot(p(1),p(2),'*r'); axis equal

% an example of the epipolar line computation
ll = F*[x1_im(1), y1_im(1), 1]'
% to get another point on the epipolar line
p2 = skew(ll)*p;
p2 = p2/p2(3)

% direction of the line in the image
ld = p - p2;
ld = ld/norm(ld);
% ploting the line ... here you may need to fix something 
% to make sure it will be in the image
line([p(1) p(1) + 10*ld(1)], [p(2) p(1) + 10*ld(2)])

pause



check = A*actual_trans'/(norm(actual_trans'))
M = skew(p)*F'/(norm(p))^2;
rank(M);
pause;
 
% solve for projective structure
P1 = [M p]
P2 = [diag([1 1 1]) zeros(3,1)]

[vec,val] = eig(M);

for i=1:NPOINTS
 C = [P1(1,:)- x1_im(i)*P1(3,:);
       P1(2,:)- y1_im(i)*P1(3,:);
       P2(1,:)- x2_im(i)*P2(3,:);
       P2(2,:)- y2_im(i)*P2(3,:)];
 [U,S,V] = svd(C);
 XP(:,i,1) = V(:,4);
 XP(:,i,1) = XP(:,i,1)/XP(4,i,1);
end

pause;
figure;  % plot projective reconstruction
%subplot(1,PLOTS,1);
plot3(XP(1,1:8,1),XP(3,1:8,1),XP(2,1:8,1),'*r');
hold on
plot3(XP(1,9:12,1),XP(3,9:12,1),XP(2,9:12,1),'*b');
plot3_struct(XP(1,:,1),XP(3,:,1),XP(2,:,1));
xlabel('x'); ylabel('z'); zlabel('y');
view(220,20);
%view(200,20);
grid off;
%axis equal;
axis off;

if affine 
  for i=1:3
     C = [P1(1,:) - van_point1(1,i)*P1(3,:);
          P1(2,:) - van_point1(2,i)*P1(3,:);
          P2(1,:) - van_point2(1,i)*P2(3,:);
          P2(2,:) - van_point2(2,i)*P2(3,:)];
          [U,S,V] = svd(C);
          VXP(:,i,1) = V(:,4);
          VXP(:,i,1) = VXP(:,i,1)/VXP(4,i,1);
  end

  VXP % projective structure of vanishing points

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

  for i=1:NPOINTS
    XA(:,i,1) = Ha*XP(:,i,1);
    XA(:,i,1) = XA(:,i,1)/XA(4,i,1);
  end

  figure;
  plot3_struct(XA(1,:,1),XA(3,:,1),XA(2,:,1));
  view(200,20);
  grid off;

end % affine structure 


% set up the measurements matrix for two frames
% to compute the structure without the calibration
% some experiments for decomposing the fundamental matrix

if 0
  FRAMES = 2;
  M = zeros(2*FRAMES,NPOINTS);
  M(1,:) = x1_im;
  M(1+FRAMES,:) = y1_im;
  M(2,:) = x2_im;
  M(2+FRAMES,:) = y2_im;

  [vel, R, pos_depth] = robust_dessential([x1;y1;ones(1,NPOINTS)], ...
                        [x2;y2;ones(1,NPOINTS)],R0,actual_trans, 1)

  M(1,:) = x1;
  M(1+FRAMES,:) = y1;
  M(2,:) = x2;
  M(2+FRAMES,:) = y2;

  p = [x1_im;y1_im;ones(1,NPOINTS)];
  q = [x2_im;y2_im;ones(1,NPOINTS)];
  F = dfundamental(p, q)
  [vel, R, pos_depth] = robust_dessential(p, q, R0, actual_trans, 1)

  Rtilde = transpose(inv(A))*R0'*(A)'
  Fa = (F - F')/2;
  %pause
  E = R*skew(vel)
  [u,s,v] = svd(E)

  S = m_structure(M,vel,R);
  figure;
  plot3_struct(S(1,:),S(3,:),-S(2,:));
  hold on;
  plot3(S(1,:),S(3,:), -S(2,:),'.');
  xlabel('x'); ylabel('z'); zlabel('y');
  view(20,20);
  grid on;
  %axis equal;

end
