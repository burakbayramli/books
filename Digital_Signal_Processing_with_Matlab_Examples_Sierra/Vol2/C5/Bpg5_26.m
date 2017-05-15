% Image HOG example

%load the images
ip=imread('Street1.jpg');
ip=ip(:,:,1); %select one plane
np=im2double(ip); %convert to float
ip=imread('Nebula1.jpg');
ip=ip(:,:,1); %select one plane
ap=im2double(ip); %convert to float

figure(1)
subplot(1,2,1)
imshow(np); title('natural image');
subplot(1,2,2)
imshow(ap); title('astronomy image');

figure(2)
%natural image
[vx,vy]=gradient(np); %gradients
mv=abs(vx+(j*vy)); %gradient amplitude
[hny]=hist(mv(:),100); %histogram

%astronomy image
[vx,vy]=gradient(ap); %gradients
mv=abs(vx+(j*vy)); %gradient amplitude
[hay]=hist(mv(:),100); %histogram

plot(hny,'k'); hold on;
plot(hay,'r');
axis([0 50 0 1.1*max(hay)]);
title('HoG of natural (black) and astronomy (red) images');

figure(3)
plot(log(hny+1),'k'); hold on;
plot(log(hay+1),'r')
title('log HoG of natural (black) and astronomy (red) images');

