% Frequency domain Wiener filtering
% Image + noise
ux=imread('antena1.tif');
[Ny,Nx]=size(ux);

%signal and noise between 0 and 1
x=im2double(ux);
vn=abs(0.4*randn(Ny,Nx)); v=mod(vn,1);

X=fftshift(fft2(x)); %Fourier transform of x

%image+noise 
Kmix=0.7; %to be edited
x=Kmix*x; v=(1-Kmix)*v;
y=x+v;

%Wiener computations
Sxx=abs(X).^2; %Sxx
V=fftshift(fft2(v)); %Fourier transform of v
Svv=abs(V).^2; %Svv
WH=Sxx./(Sxx+Svv); %Fourier transform of the Wiener filter

Y=fftshift(fft2(y)); %Fourier transform of y
fly=abs(ifft2(Y.*WH)); %apply the Wiener filter

%signals between 0 and 1
miy=min(min(y)); y=y-miy; %y>=0
may=max(max(y)); y=y/may; %y<=1
mify=min(min(fly)); fly=fly-mify; %fly>=0
mafy=max(max(fly)); fly=fly/mafy; %fly<=1

Uy=im2uint8(y); %convert to uint8 for display
Ufly=im2uint8(fly); %convert to uint8 for display


%display---------------------------
figure(1)
imshow(Uy); %plots figure
title('image+noise');

figure(2)
imshow(Ufly); %plots figure
title('filtered image');
