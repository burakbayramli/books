% Steerable pyramid

ufg=imread('holland1.jpg'); %read the 256x256 image file into a matrix
fg=double(ufg); %convert to float
N=256;
Ffg=fftshift(fft2(fg)); %2D Fourier transform of the image

%First decomposition------------------------------------------
w1=pi/2; wmax=pi;
[h0i,H0F]=stp_HI(Ffg,N,w1,wmax); %high-pass
[l0i,L0F]=stp_LI(Ffg,N,w1,wmax); %low-pass

M=256;

figure(1)
subplot(1,2,1)
ahi=abs(h0i); %only real values
nhi=(M/max(max(ahi)))*ahi; %normalize image range
uhi=uint8(M-nhi); %convert to unsigned 8-bit 
imshow(uhi); %display the filtered image
title('H0');

subplot(1,2,2)
ali=abs(l0i); %only real values
nli=(M/max(max(ali)))*ali; %normalize image range
uli=uint8(nli); %convert to unsigned 8-bit 
imshow(uli); %display the filtered image
title('L0');


%Second decomposition------------------------------------------
w0=pi/4; w1=pi/2;
[b10i,b10F]=stp_BI(L0F,N,w0,w1,0); %band-pass(0º)
[b11i,b11F]=stp_BI(L0F,N,w0,w1,2*pi/3); %band-pass(120º)
[b12i,b12F]=stp_BI(L0F,N,w0,w1,4*pi/3); %band-pass(240º)

[l1i,L1F]=stp_LI(L0F,N,w0,w1); %low-pass
L1F=L1F(1:2:end, 1:2:end);%subsampling

figure(2)
subplot(2,3,1)
ahi=abs(b10i); %only real values
nhi=(M/max(max(ahi)))*ahi; %normalize image range
uhi=uint8(M-nhi); %convert to unsigned 8-bit 
imshow(uhi); %display the filtered image
title('B 0º');
subplot(2,3,2)
ahi=abs(b11i); %only real values
nhi=(M/max(max(ahi)))*ahi; %normalize image range
uhi=uint8(M-nhi); %convert to unsigned 8-bit 
imshow(uhi); %display the filtered image
title('B 120º');
subplot(2,3,3)
ahi=abs(b12i); %only real values
nhi=(M/max(max(ahi)))*ahi; %normalize image range
uhi=uint8(M-nhi); %convert to unsigned 8-bit 
imshow(uhi); %display the filtered image
title('B 240º');

subplot(2,3,5)
ahi=abs(l1i); %only real values
nhi=(M/max(max(ahi)))*ahi; %normalize image range
uhi=uint8(nhi); %convert to unsigned 8-bit 
imshow(uhi); %display the filtered image
title('L1');




