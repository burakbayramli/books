% Algorithm 8.1. also 11.7
% Rank based factorization algorithm for multiview reconstruction  
% using point features 
% as described in Chapter 8, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

% Generates multiple synthetic views of a house and computes the 
% motion and structure, calibrated case, point features only
% Jana Kosecka, George Mason University, 2002
% ======================================================================

close all; clear;
FRAMES = 3;
PLOTS  = 3;
% transformation is expressed wrt to the camera frame

Zinit = 5;

% cube in the object frame
 XW = [0 1 1 0 0 1 1 0 0.2 0.8 0.2 0.8 ;
       0 0 1 1 0 0 1 1 1.5 1.5 1.5 1.5;
       1 1 1 1 0 0 0 0 0.8 0.8 0.2 0.2 ;
       1 1 1 1 1 1 1 1 1   1   1   1];

NPOINTS = 12; 

XC = zeros(4,NPOINTS,FRAMES);

% initial displacement
Rinit = rot_matrix([1 1 1],0); 

Tinit = [ Rinit(1,:) -0.5 ;
          Rinit(2,:) -0.5 ;
          Rinit(3,:) Zinit;
          0 0 0 1];
% first camera coodinates 
XC(:,:,1) = Tinit*XW;

figure; hold on;
plot3_struct(XC(1,:,1),XC(2,:,1),XC(3,:,1));
plot3(XC(1,:,1),XC(2,:,1),XC(3,:,1),'*');
draw_frame_scaled([diag([1,1,1]), zeros(3,1)],0.5);
title('original motion and 3D structure');
view(220,20);
grid on; axis equal;
% axis off;
pause;


% image coordinates
xim(:,:,1) = project(XC(:,:,1));

Zmax = max(XC(3,:,1));
Zmin = min(XC(3,:,1));
rinc =   pi/30;
rot_axis = [1 0 0; 0 -1 0]';
trans_axis = [1 0 0; 0 1 0]';

ratio = 1;
rinc = 10;  % rotation increment 20 degrees
Zmid = (Zmax+Zmin)/2;
tinc = 0.5*ratio*Zmid*rinc*pi/180;

ploting = 1;

for i=2:FRAMES
    theta = (i-1)*rinc*pi/180;
    r_axis = rot_axis(:,i-1)/norm(rot_axis(:,i-1));
    t_axis = trans_axis(:,i-1)/norm(trans_axis(:,i-1));
    trans = (i-1)*tinc*t_axis;
    R = rot_matrix(r_axis,theta);
    % translation represents origin of the camera frame
    % in the world frame 
    T(:,:,i) = ([ R trans;
                 0 0 0 1]);
    % all transformation with respect to the object frame
    XC(:,:,i) = T(:,:,i)*XC(:,:,1);  % XW;
    draw_frame_scaled(T(1:3,:,i),0.5); 
    xim(:,:,i) = [XC(1,:,i)./XC(3,:,i); XC(2,:,i)./XC(3,:,i); ...
                  ones(1,NPOINTS)];
end;

for j = 2:FRAMES
 T_ini(:,j) = T(1:3,4,j);
end;

% noise can be added here
for i=1:FRAMES     
  xim_noisy(:,:,i) = xim(:,:,i);
end   

% pause
%---------------------------------------------------------------------
% compute initial \alpha's for each point using first two frames only
[T0, R0]  = essentialDiscrete(xim_noisy(:,:,1),xim_noisy(:,:,2));
for i = 1:NPOINTS
  alpha(:,i) = -(skew(xim_noisy(:,i,2))*T0)'*...
      (skew(xim_noisy(:,i,2))*R0*xim_noisy(:,i,1))...
      /(norm(skew(xim_noisy(:,i,2))*T0))^2;
  lambda(:,i) = 1/alpha(:,i);
end

scale = norm(alpha(:,1));     % set the global scale
alpha = alpha/scale;          % normalize everything
scale = norm(lambda(:,1));     % set the global scale
lambda = lambda/scale;         % normalize everything

%---------------------------------------------------------------------
% Compute initial motion estimates for all frames
% Here do 3 iterations - in real setting look at the change of scales

iter = 1;
while (iter < 5);
  for j = 2:FRAMES
    P = []; %  setup matrix P
    for i = 1:NPOINTS
      a = [kron(skew(xim_noisy(:,i,j)),xim(:,i,1)') ...
	   alpha(:,i)*skew(xim_noisy(:,i,j))];
      P = [P; a];
    end;
    % pause
    [um, sm, vm] = svd(P);
    Ti = vm(10:12,12);
    Ri = transpose(reshape(vm(1:9,12)',3,3));
    [uu,ss,vv] =  svd(Ri);
    Rhat(:,:,j) = sign(det(uu*vv'))*uu*vv';
    Ti = sign(det(uu*vv'))*Ti/((det(ss))^(1/3));
    That(:,j) = Ti;
    True = T(1:3,4,j);
  end

  % recompute alpha's based on all views
  lambda_prev = lambda;
  for i = 1:NPOINTS
    M = [];  % setup matrix M
    for j=2:FRAMES       % set up Hl matrix for all m views
      a = [ skew(xim(:,i,j))*That(:,j) ...
	    skew(xim(:,i,j))*Rhat(:,:,j)*xim(:,i,1)];
      M = [M; a];
    end;
    a1 = -M(:,1)'*M(:,2)/norm(M(:,1))^2;
    lambda(:,i) = 1/a1;
  end;
  scale = norm(lambda(:,1));   % set the global scale
  lambda = lambda/scale;     % normalize everything
  iter = iter + 1
end % end while iter

% final structure with respect to the first frame
XF = [lambda.*xim(1,:,1);
      lambda.*xim(2,:,1);
      lambda.*xim(3,:,1)];

figure; hold on;
plot3(XF(1,:,1),XF(2,:,1),XF(3,:,1),'r*');
plot3_struct(XF(1,:,1), XF(2,:,1), XF(3,:,1));
title('recovered structure');
view(220,20);
grid on; axis equal;
% axis off;
pause;
