% Image denoising by minimizing 
%   the energy E(u)= int_{Omega} (I-u)^2 + alpha |grad u|^2 dx
% using standard gradient descent.
%
% which assumes 
% 1) additive noise model with Gaussian noise iid N(0,sigma_n)
% 2) prior on images: gradient components are iid N(0,sigma_p)
% 3) affine camera model
%
% alpha = sigma_n^2/sigma_p^2 is a 1 / SNR and controls the amount of 
% regularity
%
%
% Author : Ganesh Sundaramoorthi


% algorithm parameters
alpha=17;
dt=0.9;
TOL=1e-5; % tolerance of error before algorithm quits

% read image and make it gray scale
I=imread('ucla.jpg');
Iorig=rgb2gray(I);

% create iid Gaussian noise and add it to the image
I=double(imnoise(Iorig,'gaussian', 0, 0.05));

% initialize gradient descent with the image itself
uhat=I;

figure(1);
subplot(121);
imagesc(I); colormap gray;
subplot(122);
imagesc(uhat); colormap gray;

for i=0:100000,
  % Note Neumann Boundary conditions applied here
  upx = TranslateImage(uhat, -1,  0);
  umx = TranslateImage(uhat,  1,  0);
  upy = TranslateImage(uhat,  0, -1);
  umy = TranslateImage(uhat,  0,  1);

  % L = Id - alpha Laplacian
  Luhat = uhat - alpha*(upx+umx+upy+umy-4*uhat);

  % negative gradient direction
  gradE = I-Luhat;

  error=max(max(abs(gradE)))

  if error < TOL,
    break;
  end
  
  % compute stable step size using CFL condition
  dtCFL = dt*2/(1+8*alpha);
  if dtCFL > dt,
    dtCFL=dt;
  end
  
  uhat  = uhat + dtCFL * gradE;

  % make nice display
  if mod(i,5)==0,
    figure(1); 
    subplot(122);
    imagesc(uhat); colormap gray;
    str=strcat('Iteration: ',num2str(i));
    title(str);
    drawnow;
  end
end