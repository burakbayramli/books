%function [quality] = GridQuality(ima,winx,winy);
%
% evaluate the quality of the texture at each point in the image ima
% by integrating information over a window of size winx x winy
%
%support routine for 'trackdemo.m' (help trackdemo)
%
%
%Contributors to this code include: Jean-Yves Bouguet, Hailin Jin.
%Last updated 5/5/2003.
%
%DISTRIBUTED FREE FOR NON-COMMERCIAL USE
%Copyright (c) MASKS, 2003

function [quality] = GridQuality(ima,winx,winy);

% compute smoothed gradients 
% alternatively we could use 
% derivatives of the Gaussian
[grad_y, grad_x] = gradient(ima);	
% a = d^2 I / dx^2
a = conv2(conv2(grad_x .* grad_x, ones(2*winx+1,1), 'same'),ones(1,2*winy+1), 'same');
% b = d^2 I / (dx dy)
b = conv2(conv2(grad_x .* grad_y, ones(2*winx+1,1), 'same'),ones(1,2*winy+1), 'same');
% c = d^2 I / dy^2
c = conv2(conv2(grad_y .* grad_y, ones(2*winx+1,1), 'same'),ones(1,2*winy+1), 'same');

clear grad_x grad_y;

% compute eigenvalues of the symmetric matrix [a b; b c]
% l_1,2 = (a+c)/2 +/- sqrt((a+c)^2/4 - ac + b^2)
m = (a + c ) / 2;
d = a .* c - b .^ 2;
n = sqrt(m .^ 2 - d);

% quality favors regions that are "directional" (m-n)
% and with "strong" texture (m+n) 
quality = min(abs(m - n),abs(m + n));
