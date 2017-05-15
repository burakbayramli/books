% Filtering the Fourier transform and then inverse
% high-pass gaussian filter

keaton=imread('keaton2bw.tif'); %read the image file into a matrix
[ly,lx]=size(keaton); %determine image size
yo=ceil(ly/2); xo=ceil(lx/2); %figure center
xgfil=ones(ly,lx); %filter initialization
if ly>lx rcir=xo-1; fxo=xo; fyo=xo;
else rcir=yo-1; fyo=yo; fxo=yo;
end; %radius and center of the filter
sigma=10; %filter bandwidth parameter
gfil=fspecial('gaussian',1+(2*rcir),sigma); %gaussian filter
ngfil=mat2gray(gfil); %normalization
spnx=[(xo-rcir):(xo+rcir)]; %set of indexes
spny=[(yo-rcir):(yo+rcir)]; %set of indexes
spn=[(fyo-rcir):(fyo+rcir)]; %set of indexes
xgfil(spny,spnx)=1-ngfil(spn,spn); %put circular filter on image rectangle

FKe=fftshift(fft2(keaton)); %Fourier transform
FKefil=FKe.*xgfil; %filtering in the frequency domain
IKe=ifft2(FKefil); %inverse transform
uIKe=uint8(abs(IKe)); %convert to unsigned 8-bit

figure(1)
imshow(xgfil); %the filter
title('Fourier gaussian high-pass filtering');
ylabel('the filter');

figure(2)
mesh(xgfil); %3D view of the filter
title('3D view of the filter');

figure(3)
imshow(uIKe); %the filtered picture
title('filtered image');

