% Lucy-Richardson example

%load the image
ip=imread('saturn.tif');
op=im2double(ip); %convert to float

%create the PSF
[oL,oC]=size(op);
L=oL-1; C=oC-1; %number-1 of rows and columns
sigma=2;
[x,y]=meshgrid(-C/2:C/2, -L/2:L/2);
argm=-((x.^2)+(y.^2))/(2*sigma*sigma);
d=exp(argm); 
md=sum(d(:));
d=d/md; %the normalized PSF

%the degradation OTF
D=fft2(d);

%preparation for LR
mp=medfilt2(op); %median filtering
ep=mp; %initial estimated image

%the LR iterations

for nn=1:5,
   
%denominator (convolution using Fourier)
EP=fft2(ep);
BEP=D.*EP;
bep=abs(ifftshift(ifft2(BEP)));

%num/den
r=ep./(0.00001+bep); %simple regularization

%correction vector (convolution using Fourier)
R=fft2(r);
CV=D.*R;
cv=abs(ifftshift(ifft2(CV)));

%next estimate
ep=cv.*ep;

end;

%display----------------
figure(1)
imshow(op); title('original image')

figure(2)
imshow(ep); title('restored image');