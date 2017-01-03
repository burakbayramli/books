% Rank based factorization algorithm for multiview reconstruction  
% using points and lines features and their incidence relations
% as described in Chapter 9, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

% Generates multiple synthetic views of a house and computes the 
% motion and structure, calibrated case, point features only
% Jana Kosecka, George Mason University, 2002
% ==============================================================

close all; clear;
FRAMES = 3;
PLOTS  = 3;
     
% cube in the object frame
 XW = [0 1 1 0 0 1 1 0 0.2 0.8 0.2 0.8 ;
       0 0 1 1 0 0 1 1 1.5 1.5 1.5 1.5;
       1 1 1 1 0 0 0 0 0.8 0.8 0.2 0.2 ;
       1 1 1 1 1 1 1 1 1   1   1   1];

NPOINTS = 12;
XC = zeros(4,NPOINTS,FRAMES);

% initial displacement
Rinit = rot_matrix([1 1 1],0); 
Zinit = 5;
Tinit = [ Rinit(1,:) -0.5 ;
          Rinit(2,:) -0.5 ;
          Rinit(3,:) Zinit;
          0 0 0 1];
% first camera coodinates 
XC(:,:,1) = Tinit*XW;
Zmax = max(XC(3,:,1));
Zmin = min(XC(3,:,1));
rinc =   pi/30;
rot_axis = [1 0 0; 0 -1 0]';
trans_axis = [1 0 0; 0 1 0]';

figure; 
subplot(1,PLOTS,1); hold on;
plot3_struct(XC(1,:,1),XC(2,:,1),XC(3,:,1));
plot3(XC(1,:,1),XC(2,:,1),XC(3,:,1),'*');
draw_frame_scaled([diag([1,1,1]), zeros(3,1)],0.5);
title('original motion and 3D structure');
view(220,20);
grid on; axis equal;

% retinal coordinates
xr(:,:,1) = project(XC(:,:,1));
subplot(1,PLOTS,2); hold on;
plot(xr(1,:,1),xr(2,:,1),'.');
[lim(:,:,1), inc] = houselines_mixed(xr(1:2,:,1));
plotLines(lim(:,:,1));
title('retinal coordinates'); axis equal;
% pause;

ratio = 1;
rinc = 10;  % rotation increment 20 degrees
Zmid = (Zmax+Zmin)/2;
tinc = 0.5*ratio*Zmid*rinc*pi/180; 

for i=2:FRAMES
    theta = (i-1)*rinc*pi/180;
    r_axis = rot_axis(:,i-1)/norm(rot_axis(:,i-1));
    t_axis = trans_axis(:,i-1)/norm(trans_axis(:,i-1));
    T_ini(:,i) = (i-1)*tinc*t_axis;
    R = rot_matrix(r_axis,theta);
    % translation represents origin of the camera frame in the world frame 
    T(:,:,i) = ([ R T_ini(:,i);
                 0 0 0 1]);
    XC(:,:,i) = T(:,:,i)*XC(:,:,1);  % XW;
    xr(:,:,i) = project(XC(:,:,i));
    [lim(:,:,i),inc] = houselines_mixed(xr(1:2,:,i));

    subplot(1,PLOTS,1); hold on;
    draw_frame_scaled(T(:,:,i),0.5); axis equal; 
    subplot(1,PLOTS,2);  
    plot(xr(1,:,i),xr(2,:,i),'.'); axis equal;
    plotLines(lim(:,:,i));
end;

lnum = size(lim,2);
for i = 1:FRAMES
  for j = 1:lnum 
    l =  skew([lim(1:2,j,i);1])*[lim(3:4,j,i); 1];
    ln(:,j,i) = l/norm(l);
  end
end

%------------------------------------------------------------
% MOTION ESTIMATION - given m view n lines - estimate motion 
for i = 1:FRAMES
    [lim(:,:,i),inc] = houselines_mixed(xr(1:2,:,i));      
end

%---------------------------------------------------------------------
% compute initial \alpha's for each point using first two frames only
[vel,R0]  = essentialDiscrete(xr(:,:,1),xr(:,:,2));
R_ini(:,:,2) = R0;
T_ini(:,2) = vel;

%-----------------------------------------
% compute alpha's using initial estimates
for i = 1:NPOINTS
    alpha(:,i) = -(skew(xr(:,i,2))*T_ini(:,2))'*(skew(xr(:,i,2))*R_ini(:,:,2)* ...
                  xr(:,i,1))/(norm(skew(xr(:,i,2))*T_ini(:,2)))^2;
    lambda(:,i) = 1/alpha(:,i);
end
scale = norm(alpha(:,1));     % set the global scale
alpha = alpha/scale;          % normalize everything

%---------------------------------------------------------------------
% Compute initial motion estimates for all frames
% Here do 3 iterations - in real setting look at the change of scales 

iter = 1;
while (iter < 5)
     for j = 2:FRAMES
	 P = []; %  setup matrix P
	for i = 1:NPOINTS
	    a = [lambda(:,i)* kron(skew(xr(:,i,j)), ...
                        xr(:,i,1)') skew(xr(:,i,j))];
            P = [P; a];
            % add all the entries of lines incident to the point
            sl = size(inc(i).lines,2);
        end;
        % pause
        [um, sm, vm] = svd(P);
	Ti = vm(10:12,12);
	Ri = transpose(reshape(vm(1:9,12)',3,3));
	[ur,sr,vr] =  svd(Ri);
	Rhat(:,:,j) = sign(det(ur*vr'))*ur*vr';
        Ti = sign(det(ur*vr'))*Ti/((det(sr))^(1/3));
        scaled_init_T = T_ini(:,j)/scale;
        That(:,j) = Ti;
        True = T(1:3,4,j);
     end
       
     %---------------------------------------
     % recompute alpha's based on all views
     lambda_prev = lambda;
     for i = 1:NPOINTS
        M = [];  % setup matrix M
        for j = 2:FRAMES       % set up Hl matrix for all m views
	     a = [skew(xr(:,i,j))*Rhat(:,:,j)*xr(:,i,1) skew(xr(:,i,j))*That(:,j)];
             M = [M; a];
             sl = size(inc(i).lines,2);
             for s = 1:sl
               li = inc(i).lines(s);
               a = [ln(:,li,j)'*Rhat(:,:,j)*xr(:,i,1) ln(:,li,j)'*That(:,j)];
               M = [M; a];
             end;
         end;
         lambda(:,i) = -M(:,1)'*M(:,2)/norm(M(:,1))^2;
      end;
      scale = norm(lambda(:,1));   % set the global scale
      lambda = lambda/scale;     % normalize everything
      iter = iter + 1;
end % end while iter

% final structure with respect to the first frame
XF = [lambda.*xr(1,:,1);
      lambda.*xr(2,:,1);
      lambda.*xr(3,:,1)];

subplot(1,PLOTS,3); hold on;
plot3(XF(1,:,1),XF(2,:,1),XF(3,:,1),'r*');
plot3_struct(XF(1,:,1), XF(2,:,1), XF(3,:,1));
title('recovered structure');
view(220,20);
grid on; axis equal;

