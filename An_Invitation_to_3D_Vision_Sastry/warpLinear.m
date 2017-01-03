% warps the image according to the transformation Hm
% transformation is around image origin and all the 
% and the image is resampled on the same grid
function[image] = warpLinear(Hm, im)
[s1, s2] = size(im);
range_x = (1:s1);
range_y = (1:s2);
[x, y] = meshgrid(range_y-s2/2,range_x-s1/2); % original image 
[xo, yo] = meshgrid(range_y,range_x);

xn = reshape(x,[1,s1*s2]);
yn = reshape(y,[1,s1*s2]);
gn = [xn; yn; ones(1,s1*s2)];

% transformed coordinates
ww = Hm*gn;
wx = ww(1,:)./ww(3,:) + s1/2;
wy = ww(2,:)./ww(3,:) + s2/2;

% new grid
xi = reshape(wx,[s1,s2]);
yi = reshape(wy,[s1,s2]);
zi = interp2(xo,yo,double(im),xi,yi); 

image = uint8(zi);

