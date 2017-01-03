% May 2003, Jana Kosecka, George Mason University
function[im1, x2, y2] = Hwarp(H, im0)
% backward warp 

[ydim, xdim] = size(im0);
% ydim = ydim/2;
% xdim = xdim/2;

ulc = H*[1,1,1]';
ulc = ulc/ulc(3);
urc = H*[xdim,1,1]';
urc = urc/urc(3);
llc = H*[1,ydim,1]'; llc = llc/llc(3);
lrc = H*[xdim,ydim,1]'; lrc = lrc/lrc(3);

%  [ulc(1),llc(1),urc(1),lrc(1)];
%  [ulc(2),urc(2),llc(2),lrc(2)];

% compute the new meshgrid 
xmin = min([ulc(1),llc(1),urc(1),lrc(1)]);
xmax = max([ulc(1),llc(1),urc(1),lrc(1)]);
ymin = min([ulc(2),urc(2),llc(2),lrc(2)]);
ymax = max([ulc(2),urc(2),llc(2),lrc(2)]);

% generate coordinates in the new image  
range_x = xmin:xmax;
range_y = ymin:ymax;
[x2, y2] = meshgrid(range_x,range_y); % original image 
[ydim, xdim] = size(x2);
xx = reshape(x2,[1,ydim*xdim]);
yy = reshape(y2,[1,ydim*xdim]);
gg = [xx; yy; ones(1,ydim*xdim)];
ww = H \ gg;

wx = ww(1,:)./ww(3,:); 
wy = ww(2,:)./ww(3,:); 
xi = reshape(wx,[ydim,xdim]);
yi = reshape(wy,[ydim,xdim]);
im1 = interp2(double(im0),xi,yi,'bilinear'); 


