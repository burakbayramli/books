% Image gradients example

%load the image
ip=imread('Head.jpg');
op=im2double(ip); %convert to float

figure(1)
imshow(ip);

figure(2)
N=90; H=42; V=40;
lp=op(V:V+N,H:H+N); %select part
[L,C]=size(lp);
[x,y]=meshgrid(1:1:L,C:-1:1);

[vx,vy]=gradient(lp);
mv=abs(vx+(j*vy));
surf(x,y,mv)
view(-5.5,86);
title('3D view of the gradients');

figure(3)
N=34; H=94; V=62;
lp=op(V:V+N,H:H+N); %select part
[L,C]=size(lp);
[x,y]=meshgrid(1:1:L,C:-1:1);

[vx,vy]=gradient(lp);
quiver(x,y,vx,vy,'k');
axis([1 N 1 N]);
title('Zoom on nose gradients');

