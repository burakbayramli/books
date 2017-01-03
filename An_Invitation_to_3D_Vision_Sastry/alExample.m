clear; close all;
im = imread('al.tiff','tif');
[ydim, xdim] = size(im);
im = im(3:xdim-2, 3:ydim-2);
imagesc(im); colormap gray; axis off; axis equal;

prefilt = [0.223755 0.552490 0.223755];
derivfilt = [-0.453014 0 0.45301];
blur	= [1 6 15 20 15 6 1];
blur	= blur / sum(blur);

imblurr = conv2( conv2( im, blur', 'same' ), blur, 'same' );
fx	= conv2( conv2( im, prefilt', 'same' ), derivfilt, 'same' );
fy	= conv2( conv2( im, derivfilt', 'same' ), prefilt, 'same' );
figure; 
imagesc(imblurr); colormap gray; axis off; axis equal;
figure;
imagesc(fx); colormap gray; axis off; axis equal;
figure;
imagesc(fy); colormap gray; axis off; axis equal;

BW = edge(im,'canny');
figure;
imagesc(BW); colormap gray; axis off; axis equal;


