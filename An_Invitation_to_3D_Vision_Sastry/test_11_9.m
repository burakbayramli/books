% Example for alg. 11.9  "An introduction to 3-D Vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry (MASKS)
% Code distributed free for non-commercial use
% Copyright (c) MASKS, 2003

% Projective rectification example
% Given fundamental matrix between two views compute 
% warping homographies H1, H2 for both views.
% See step-by-step.m for use in an example.
% May 2003, Jana Kosecka, George Mason University
% ================================================

% loads x1, x2 and F
load rectifData;

im0 = imread('oldhouse2/A2000000.bmp');
im1 = imread('oldhouse2/A2000084.bmp');
[ydim, xdim] = size(im0);

[H1, H2] = projRectify(F,x1, x2, xdim, ydim)
 Tr = [1 0 -xdim/2; 0 1 -ydim/2 ; 0 0 1]; 
 H1 = inv(Tr)*H1;
 H2 = inv(Tr)*H2;
 xim1r = project(H1*x1);
 xim2r = project(H2*x2);
 [im0w, xi0, yi0] = Hwarp(H1, im0); 
 [im1w, xi1, yi1] = Hwarp(H2, im1);
 figure;
 imagesc(im0w); colormap gray; hold on; axis image;
 figure; 
 imagesc(im1w); colormap gray; hold on; axis image;
 
