function  [maxima] = LocalMax(image)
%
% LOCALMAX	Finds local maxima of image
%
%	[maxima,i,j] = LocalMax(image)
%
%	outputs array maxima of zeros and ones with ones
%	at maxima position (Extended to 9 points by JYB)
%
%support routine for 'trackdemo.m' (help trackdemo)
%
%
%Contributors to this code include: Jean-Yves Bouguet, Hailin Jin, Paolo Favaro.
%Last updated 5/5/2003.
%
%DISTRIBUTED FREE FOR NON-COMMERCIAL USE
%Copyright (c) MASKS, 2003

[m,n] = size(image);
center_cols = [1:n];
center_rows = [1:m];
left = [2:n n];
right = [1 1:n-1];
top = [2:m m];
bot = [1 1:m-1];
maxima = ( ( (image(center_rows,center_cols) > image(center_rows,left)) & ...
	(image(center_rows,center_cols) > image(center_rows,right)) & ... 
	(image(center_rows,center_cols) > image(top,center_cols)) & ...
	(image(center_rows,center_cols) > image(bot,center_cols)) & ...
	(image(center_rows,center_cols) > image(top,left)) & ...
	(image(center_rows,center_cols) > image(top,right)) & ...
	(image(center_rows,center_cols) > image(bot,left)) & ...
	(image(center_rows,center_cols) > image(bot,right))));


