% nonlinear Warp
clear;
im = double(imread('lady.gif'));
[ydim,xdim] = size( im );
% im1	= im1 - min(im1(:));
% im1	= im1 / max(im1(:));
% im2	= im2 - min(im2(:));
% im2	= im2 / max(im2(:));
% im3	= im3 - min(im3(:));
% im3	= im3 / max(im3(:));
K	= -0.3;
[x,y]	= meshgrid( 2/xdim*([1:xdim]-xdim/2), 2/ydim*([1:ydim]-ydim/2) );
R 	= sin(x.^2 + y.^2);
x2 	= x .* (1 - K*R);
y2 	= y .* (1 - K*R);
alpha	= max( max(x2(:)), max(y2(:)) );
x2	= x2 / alpha;
y2	= y2 / alpha;
im_out	= interp2( x,y,im,x2,y2,'cubic' );
ind    = find( isnan(im_out) );	% FIND NANs
im_out(ind) = 0;	
		% REPLACE WITH ZEROS
im_out		= im_out - min(im_out(:));
im_out		= im_out / max(im_out(:));
im_out          = im_out(2:ydim,2:xdim,:);
imagesc(im_out, [0 1]); axis image off; colormap gray;



