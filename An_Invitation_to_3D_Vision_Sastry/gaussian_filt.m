% makes zero-mean gaussian filter by taking sz samples from gaussian distribution
% sz is a vector with dimensions of the filter

sz = 5;
x = -2:2;
sigma = 1;
% 1D gaussian
g1 = exp(-x.^2/2);
% 1D gaussian normalized
g1n = g1./sum(g1);

% 2D gaussian - obtained as an outer product
g2 = g1'*g1;
% 2D gaussian normalized
g2n = g2./sum(g2(:));

% 2D gaussian obtained by sampling 2D gaussian function
cov = [1,1];
[xramp,yramp] = meshgrid([1:sz]-3,[1:sz]-3);
e = xramp.^2/(-2 * cov(2)) + yramp.^2/(-2 * cov(1));
g2s = exp(e);
g2sn = g2s./sum(g2s(:));

gd1 = -x.*exp(-x.^2/2);
% gd1n = gd1/sum(gd1);

gd2 = (x.^2/sigma^2 - 1/sigma^2).*exp(-x.^2/2);
% gd1n = gd1/sum(gd1);


prefilt = g1n;
derivfilt = gd1;

f1 = imread('lady.gif');

% compute derivatives in x and y, prefilter in the other dimension
fx1	= conv2( conv2( f1, prefilt', 'same' ), derivfilt, 'same' );
fy1	= conv2( conv2( f1, prefilt, 'same' ), derivfilt', 'same' );

% second derivatives
fx2	= conv2( conv2( fx1, prefilt', 'same' ), derivfilt, 'same' );
fy2	= conv2( conv2( fy1, prefilt, 'same' ), derivfilt', 'same' );
fxy2	= conv2( conv2( fx1, prefilt, 'same' ), derivfilt', 'same' );
fyx2	= conv2( conv2( fy1, prefilt', 'same' ), derivfilt, 'same' );

% laplacian of Gaussian
loGa = (1/(pi*cov(1)^4))*((1/cov(1)^2)*(xramp.^2 + yramp.^2) - 1).*exp(xramp.^2/(-2 * cov(2)) + yramp.^2/(-2 * cov(1)));
loGa = loGa./sum(loGa(:));

floga = conv2(f1, loGa, 'same');



fsmooth1 = conv2( conv2( f1, prefilt', 'same'), prefilt, 'same');
fsmooth2 = conv2( f1, g2sn, 'same'); 

imagesc(fsmooth1); colormap gray; axis image;
figure; 
imagesc(fsmooth2); colormap gray; axis image;
