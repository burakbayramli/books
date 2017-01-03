% Image denoising by minimizing 
%   the energy E(u) = int_{Omega} (I-u)^2 + alpha |grad u| dx
% aka as (total variation) TV denoising using the fast algorithm of
% Chambolle 2004.
%
% Author : Ganesh Sundaramoorthi

% algorithm parameters
alpha=75; % amount of smoothness
dt=1/4;   % step size for 
TOL=1e-5; % tolerance of error before algorithm quits

% read image and make it gray scale
I=imread('ucla.jpg');
Iorig=rgb2gray(I);

% create iid Gaussian noise and add it to the image
I=double(imnoise(Iorig,'gaussian', 0, 0.05));

% initialize dual gradient descent with the image itself
uhat=I;
p(:,:,1)=0*I;
p(:,:,2)=0*I;

figure(1);
subplot(121);
imagesc(I); colormap gray;
subplot(122);
imagesc(uhat); colormap gray;

for i=1:1000,
  p = chambolle_proj(p,I,dt,10,alpha);
  
  p1px=TranslateImage(p(:,:,1), -1,  0);
  p1mx=TranslateImage(p(:,:,1),  1,  0);
  p2py=TranslateImage(p(:,:,2),  0, -1);
  p2my=TranslateImage(p(:,:,2),  0,  1); 
  divp=(p1px-p1mx+p2py-p2my)/2;
  
  uhat=I-divp*alpha;
  
  upx=TranslateImage(uhat, -1,  0);
  umx=TranslateImage(uhat,  1,  0);
  upy=TranslateImage(uhat,  0, -1);
  umy=TranslateImage(uhat,  0,  1);
  
  E = sum( sum ( (I-uhat).^2/(2*alpha) + ...
		 sqrt( ( upx-umx).^2 + (upy-umy).^2 )/2 ) )

  
  % make nice display
  if mod(i,1)==0,
    figure(1); 
    subplot(122);
    imagesc(uhat); colormap gray;
    str=strcat('Iteration: ',num2str(i));
    title(str);
    drawnow;
  end
end