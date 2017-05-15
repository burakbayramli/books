% Convert Parrot to indexed image
parrot=imread('parrot1.jpg'); %read the image file into a matrix
[Ip,Cmap]=rgb2ind(parrot,64); %make a colormap with 64 entries
figure(1)
imshow(Ip,Cmap); %display color picture
title('indexed image');

disp('parrot size')
size(parrot)
disp('indexed parrot size')
size(Ip)
disp('colormap size')
size(Cmap)