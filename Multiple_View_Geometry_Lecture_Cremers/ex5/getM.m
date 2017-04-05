function [ xx, yy, xy ] = getM( img, std )

sz = 4*std+1;


% calc image gradients
dx = zeros(size(img));
dy = zeros(size(img));
dx(:,2:(end-1)) = 0.5*(img(:,3:end) - img(:,(1:end-2)));
dy(2:(end-1),:) = 0.5*(img(3:end,:) - img((1:end-2),:));

% create gaussian kernel
d = (repmat(-sz : sz, 2*sz+1,1).^2 + (repmat(-sz : sz, 2*sz+1,1).^2)').^0.5;
kernel = normpdf(d, zeros(size(d)), std*ones(size(d)));

% calc tensor content
xx = conv2(dx .*dx, kernel, 'same');
yy = conv2(dy .*dy, kernel, 'same');
xy = conv2(dx .*dy, kernel, 'same');


end

