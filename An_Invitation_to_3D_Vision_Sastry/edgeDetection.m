% Algorith 4.3 edge detection 
% Step-by-step euclidean reconstruction algorithm from multiple views
% as described in Chapter 4, "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

clear; close all;
im = double(imread('al.tif','tif'));
[ydim, xdim] = size(im);
im = im(3:xdim-2, 3:ydim-2);
imagesc(im); colormap gray; axis off; axis equal;
title('original image');

prefilt = [0.223755 0.552490 0.223755];
derivfilt = [-0.453014 0 0.45301];
blur	= [1 6 15 20 15 6 1];
blur	= blur / sum(blur);

imblurr = conv2( conv2( im, blur', 'same' ), blur, 'same' );
fx	= conv2( conv2( im, prefilt', 'same' ), derivfilt, 'same' );
fy	= conv2( conv2( im, derivfilt', 'same' ), prefilt, 'same' );
magn = sqrt(fx.^2 + fy.^2);
figure; 
imagesc(magn); title('gradient magnitude'); colormap gray;
figure; 
imagesc(imblurr); title('blurred original');
colormap gray; axis off; axis equal;
figure;
imagesc(fx); title('x-derivative');
colormap gray; axis off; axis equal;
figure;
imagesc(fy); title('y-derivative');
colormap gray; axis off; axis equal;

BW = edge(im,'canny');
figure;
imagesc(BW); colormap gray; axis off; axis equal;
title('canny edges');

