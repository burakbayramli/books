% Algorithms 11.6. , 11.7, 11.8
% Step-by-step euclidean reconstruction algorithm from multiple views
% as described in Chapter 11, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003
%
% Last modified 5/5/2005

% Following shell loads tracked point features
% and corresponding frames and computes the motion and 
% 3D structure of tracked features and focal lenght of the camera
% the skew and center of projection of the calibration matrix 
% is assumed to be known

% 1. Given n features in m view
% 2. Compute fundamental matrix and projective reconstuction from 2 views
% 3. Use rank based factorization for multiview projective reconstruction 
% 4. Solve for uknown focal lenght using absolute quadric constraints
% 5. Upgrade projective structure to euclidean one
% 6. Compute rectifying transformations
% 7. Warp the first and last view of the sequence
% ==================================================================

close all; clear;

% with the affine tracker and undone radial distortion
seq_name = 'oldhouse2/A2000';
image_type = 'bmp';
load oldhouse2/A2000_result_ST;  % tracks from 110 to 200

% specify index of starting frame fs, end frame fe 
% and the subsampling factor ft
fs = 0; fe = 88; ft = 12;
findex = [fs:ft:fe]; offset = 1;
indf = 1:size(findex,2);

% find features tracked in all the frames
ind = find(goodfeat ~= 0); 
j = 0; 
for i = (1+offset-1):ft:(fe-fs+1)
   j = j+1;
   xim(1,:,j) = xttfirst(2,ind) + SaveSTB(2,ind,i);
   xim(2,:,j) = xttfirst(1,ind) + SaveSTB(1,ind,i);
   xim(3,:,j) = 1;
end;

% consider only indf frames
xim = xim(:,:,indf);
[s, n, m] = size(xim);   

opt = '%03d'; 
imfile = sprintf('%s%s.%s',seq_name,sprintf(opt,findex(1)),image_type)
seq(1).im = (imread(imfile));

% read images
for i = 2:m 
  if findex(1) < 10 opt = '%03d'; 
  elseif findex(1) < 100 opt = '%02d'; 
  else  opt = '%01d'; 
  end;
  imfile = sprintf('%s%s.%s',seq_name,sprintf(opt,findex(i)),image_type)
  seq(i).im = imread(imfile);
end;


[ydim,xdim,cdim] = size(seq(1).im);

for i = 1:m
  imagesc(seq(i).im); colormap gray; hold on; 
  axis equal; axis image; axis off; 
  plot(xim(1,:,i), xim(2,:,i),'y.'); 
 for k = 1:n
   t = text(xim(1,k,i)+3, xim(2,k,i)+3,num2str(k)); 
   set(t, 'Color', 'yellow');
 end
end

   disp('tracked features - press ENTER to continue');
pause

%-------------------------------------------------------------
% Structure and motion and focal length recovery given xim

% guess intrinsic parameter matrix
[s, n, m] = size(xim);   
fguess = 700; % max(xdim,ydim);
Aguess = [fguess 0 xdim/2; 0 fguess ydim/2; 0 0 1];

%----------------------------
% Normalize the measurements
for i = 1:m
   xn(:,:,i) = inv(Aguess)*xim(:,:,i);   % partially calibrated views
end

%--------------------------------------------
% uncalibrated case - two view initialization
% compute F and decompose it
Rhat(:,:,1) = diag([1 1 1]);
That(:,1) = zeros(3,1);
F = dfundamental(xn(:,:,1), xn(:,:,m))
[uf, sf, vf] = svd(F'); ep = vf(:,3);
M = skew(ep)'*F; %  + rand(3,1)*ep';
Rhat(:,:,m) = M;
That(:,m) = ep;

%----------------------------
% compute projective stucture 
[X,lambda] = compute3DStructure(xn(:,:,1),xn(:,:,m),Rhat(:,:,m),That(:,m));
alpha = 1./lambda; 
alpha = alpha/alpha(1); 

%--------------------------------
% plot projective reconstruction
figure; hold on;
plot3(X(1,:,1),X(2,:,1),X(3,:,1),'k.'); 
xlabel('x'); ylabel('y'); zlabel('z');
draw_scale = X(3,1,1)/10;
Ts = That(:,1)*draw_scale;
plot3(Ts(1),Ts(2),Ts(3),'r.','MarkerSize',14);
Ts = That(:,m)*draw_scale;
plot3(Ts(1),Ts(2),Ts(3),'r.','MarkerSize',14);
title('Projective reconstruction from two views'); 
view(220,20); box on; grid off; axis equal; 
negative_depth = (~isempty(find(lambda < 0))) 

disp('end two view initialization - press ENTER');
pause;

%------------------------------------------------
% Compute initial motion estimates for all frames
init_error = 10; fs = [100]; ns = [];
errabs = init_error; err_prev = 500; iter = 1; errrel = init_error;

while (errabs > 1e-4) &  (iter < 30)  &  (errrel > 1e-5) % (errlambda > 1e-4) 
  for k = 2:m
     j = indf(k);
     Q = []; %  setup matrix P
     for i = 1:n
       Q = [Q; kron(skew(xn(:,i,j)),xn(:,i,1)') alpha(i)*skew(xn(:,i,j))];
     end;
     [um, sm, vm] = svd(Q);
     That(:,j) = vm(10:12,12);
     Rhat(:,:,j) = reshape(vm(1:9,12),3,3)';
  end;
  %--------------------------------------
  % recompute alpha's based on all views 
  lambda_prev = lambda;
  % recompute alpha's
  for i = 1:n
     M = [];  % setup matrix M
     for k = 2:m        % set up Hl matrix for all m views
       j = indf(k);
       a = [ skew(xn(:,i,j))*That(:,j) skew(xn(:,i,j))*Rhat(:,:,j)*xn(:,i,1)];
       M = [M; a];
     end;
     alpha(i) = -M(:,1)'*M(:,2)/norm(M(:,1))^2;
   end;
   scale = alpha(1);
   alpha = alpha/scale; % set the global scale
   lambda = 1./alpha;
   X = [lambda.*xn(1,:,1); lambda.*xn(2,:,1); lambda.*xn(3,:,1); ones(1,n)];
   res = [];
   i = 1;
   for l = 1:m
      j = indf(l);
      P(i*3-2:i*3,:) = [Rhat(:,:,j) scale*That(:,j)];
      tt = P(i*3-2:i*3,:)*X;
      if sum(sign(tt(3,:))) < -n/2
        P(i*3-2:i*3,:) = -[Rhat(:,:,j) scale*That(:,j)];
        Rhat(:,:,j) = -Rhat(:,:,j);
        That(:,j) = -That(:,j);
        tt = P(i*3-2:i*3,:)*X;
      end;
      xr(:,:,i) = Aguess*project(tt);
      xd = xim(1:2,:,j) - xr(1:2,:,i);
      errnorm = sqrt(xd(1,:).^2 + xd(2,:).^2);
      res = [res, errnorm];
      i = i+1;
   end

   f = sum(res)/(n*m);
   iter = iter + 1;
   fs = [fs f];
   if iter > 1 
     errrel = norm(fs(iter-1) - f);
     if fs(iter - 1) < f 
       errrel = 1e-10; 
     end;
   end;
   errabs = norm(f);
   errlambda = norm(lambda_prev-lambda);
   lambda_prev = lambda;
end % end while iter

disp('end rank based factorization - press ENTER'); 
pause;

clear xres;  
res = [];
%-------------------
% final reprojection
for i = 1:m
    j = indf(i);
    XP(:,:,i) = P(i*3-2:i*3,:)*X;
    xres(:,:,i)  = Aguess*project(XP(:,:,i));
    PC(:,:,i) = [Rhat(:,:,i) That(:,i)];
    xd = xim(1:2,:,j) - xres(1:2,:,i);
    errnorm = sqrt(xd(1,:).^2 + xd(2,:).^2);
    res(i) = sum(errnorm)/(m*n);
end

figure; hold  on; 
plot3(2*X(1,:),2*X(2,:),2*X(3,:),'k.'); box on; view(220,20);
title('Projective reconstruction from multiple views');
xlabel('x'); ylabel('y'); zlabel('z');
for i = 1:m
   Ts = That(:,i)*2*i*scale;
   plot3(Ts(1),Ts(2),Ts(3),'r.','MarkerSize',14); 
end

disp('projective reconstruction from multiple views - press ENTER');
pause;

%------------------------------------------------------------
% self-calibration - linear algorithm for unknown focal length
% set up the constraints on absolute quadric

Omega = quadric_linear_f(PC);
if Omega(1,1) < 0; 
    Omega = - Omega
end

for k = 1:m
    t = PC(:,:,k)*Omega*PC(:,:,k)';
    fest(k) = sqrt(t(1,1)/t(3,3));
    fest2(k) = sqrt(t(2,2)/t(3,3));
    Atmp(:,:,k) = Aguess*[fest(k) 0 0; 0 fest(k) 0; 0 0 1];
end

%-------------------------------------------------------
% Estimate the projective to euclidean upgrade transf Hp

v = -[Omega(1,4)/(fest(1)^2); Omega(2,4)/(fest2(1)^2); Omega(3,4)]; 
K1 = [fest(1) 0 0; 0 fest2(1) 0 ; 0 0 1];
v4 = 1;
Hp = [K1 zeros(3,1);  -v'*K1  v4];

%------------------------------------
% update the structure to Euclidean 

Xe = inv(Hp)*X; 
Xe = [Xe(1,:,1)./Xe(4,:,1);Xe(2,:,1)./Xe(4,:,1);Xe(3,:,1)./Xe(4,:,1);ones(1,size(X,2))];
if (sum(sign(Xe(3,:,1))) < 0) & (sum(sign(Xe(3,:,1))) == -size(Xe,2)) 
    Hp = [K1 zeros(3,1);  -v'*K1  -v4];
    Xe = inv(Hp)*X;
    warning('Flipping the sign of Hp');
elseif (sum(sign(Xe(3,:,1))) < 0) & (sum(sign(Xe(3,:,1))) ~= -size(Xe,2)) 
    error('Projective upgrade: some of the depths are negative');
end;

%-------------------------------    
% update the projection matrices 
PP = P*Hp; 

%------------------------------------------------------
% rigid body motion and 3D Euclidean structure recovery

for i = 1:m
   k = indf(i);
   Re(:,:,i) =  PP(i*3-2:i*3,1:3);
   [r,q] = rq(Re(:,:,i));
   Arem(:,:,i) = r; % /r(3,3); % calibration matrix 
   Re(:,:,i) = q; % rotation matrix 
   Te(:,i) =  inv(Arem(:,:,i))*PP(i*3-2:i*3,4);
   Xe(:,:,i) = [Re(:,:,i) Te(:,i); 0 0 0 1]*[Xe(:,:,1)];
end;

%------------------------------
% plot euclidean reconstruction
figure;  
gs = 100; % global scale
plot3(gs*Xe(1,:,1),gs*Xe(2,:,1),gs*Xe(3,:,1),'k.');  hold on; box on;
xlabel('x'); ylabel('y'); zlabel('z');
draw_scale = gs*Xe(3,1,1)/6;
for i=1:size(Xe,2)
    text(gs*Xe(1,i,1)+2, gs*Xe(2,i,1)+2, gs*Xe(3,i,1), num2str(i)); 
end;
title('Euclidean reconstruction from multiple views'); 
view(220,20); grid off; axis equal; %  axis off; 
for i=1:4:m
    draw_frame_scaled([Re(:,:,i)' gs*(-Re(:,:,i)'*Te(:,i))], draw_scale);
end
axis equal; box on; % view(-8,-60);

disp('Euclidean reconstruction complete - press ENTER');
pause

%-------------------------------------------------------------
% Here should go nonlinear refinement to improve the estimates
%-------------------------------------------------------------

%------------------------------------------
% estimate rectifying transformations H1, H2
Fr = dfundamental(xim(:,:,1), xim(:,:,m));
im0 = seq(1).im;
im1 = seq(m).im;

for i=1:n
   epil2r(:,i) = Fr*xim(:,i,1);
   epil1r(:,i) = Fr'*xim(:,i,m);
end;

[H1, H2] = projRectify(Fr,xim(:,:,1), xim(:,:,m), xdim, ydim)
Tr = [1 0 -xdim/2; 0 1 -ydim/2 ; 0 0 1]; 
H1 = inv(Tr)*H1;
H2 = inv(Tr)*H2;
xim1r = project(H1*xim(:,:,1));
xim2r = project(H2*xim(:,:,m));
epil1rw = inv(H1)'*epil1r;
epil2rw = inv(H2)'*epil2r;

%-------------------------------------------------
% warped the two views such that the epipolar lines 
% correpond to scanlines
[im0w, xi0, yi0] = Hwarp(H1, im0); 
[im1w, xi1, yi1] = Hwarp(H2, im1);
[ydim0, xdim0] = size(im0w);
[ydim1, xdim1] = size(im1w);
f1 = figure; imagesc(im0w); colormap gray; hold on; axis image;
f2 = figure; imagesc(im1w); colormap gray; hold on; axis image;

%------------------------
% plot few epipolar lines
indl = [1, 3, 26, 61];
for k = 1:size(indl,2);
    i = indl(k);
    figure(f1); hold on; 
    plotLineNormal(epil1rw(:,i),xdim0,ydim0); 
    axis([1 xdim0 1 ydim0]);
    figure(f2); hold on; 
    plotLineNormal(epil2rw(:,i),xdim1,ydim1);
    axis([1 xdim1 1 ydim1]);
end;


disp('warped first and last view - END');




