function [ velx, vely ] = getFlow( img, img2, std )
sz = 4*std+1;

% calc image gradients
% alternative: [dx,dy] = gradient(img);
dx = zeros(size(img));
dy = zeros(size(img));
dx(:,2:(end-1)) = 0.5*(img(:,3:end) - img(:,(1:end-2)));
dy(2:(end-1),:) = 0.5*(img(3:end,:) - img((1:end-2),:));


% calc "time" gradient
dt = img2 - img;


% create gaussian kernel
% alternative: fspacial(...)
d = (repmat(-sz : sz, 2*sz+1,1).^2 + (repmat(-sz : sz, 2*sz+1,1).^2)').^0.5;
kernel = normpdf(d, zeros(size(d)), std*ones(size(d)));



% calc tensor content
xx = conv2(dx .*dx, kernel, 'same');
yy = conv2(dy .*dy, kernel, 'same');
xy = conv2(dx .*dy, kernel, 'same');
xt = conv2(dx .*dt, kernel, 'same');
yt = conv2(dy .*dt, kernel, 'same');

% calc flow (with loop this time).
velx = zeros(size(img));
vely = zeros(size(img));

for x=1:size(img,2)
    for y=1:size(img,1)
        M = [xx(y,x) xy(y,x);xy(y,x) yy(y,x)];
        vel = M^-1 * [xt(y,x);yt(y,x)];
        velx(y,x) = vel(1);
        vely(y,x) = vel(2);
    end
end


end

