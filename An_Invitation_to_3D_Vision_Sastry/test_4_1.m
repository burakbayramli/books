im = rgb2gray(imread('shelves.jpg'));
im = double(im);
dtt = harrisCorner(im);
imagesc(im); hold on; colormap gray;
plot(dtt(2,:), dtt(1,:), 'y+');

