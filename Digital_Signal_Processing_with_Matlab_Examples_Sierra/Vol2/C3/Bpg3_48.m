% Effect of fan filter
% filtering on 2-D Fourier domain
% 
phot=imread('London1.tif'); %read the 256x256 image file into a matrix

N=256;

%Fan mask (0 or 1 values)
fmask=zeros(N,N);
L=N/2;
for nh=1:L,
   nv=1:nh;
   fmask(L+nv,L+nh)=1;
end;   
fmask((L+1):N,L:-1:1)=fmask((L+1):N,(L+1):N);
fmask(L:-1:1,L:-1:1)=fmask((L+1):N,(L+1):N);
fmask(L:-1:1,(L+1):N)=fmask((L+1):N,(L+1):N);

%filtering (Fourier domain)
Fph=fftshift(fft2(phot));
Fphfil=Fph.*fmask;
Iph=ifft2(Fphfil);
uIp=uint8(abs(Iph));

%display
figure(1)
ufmask=256*(1-fmask); %color inversion
imshow(ufmask);
title('Fan filter support');

figure(2)
imshow(uIp);
title('Filtered image');