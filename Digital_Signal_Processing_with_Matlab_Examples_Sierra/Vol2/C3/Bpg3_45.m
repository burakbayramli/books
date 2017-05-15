% Image downsampling (Quincux) example
% 
phot=imread('London1.tif'); %read the 256x256 image file into a matrix

N=256; L=N/2;

aphot=uint8(255*ones(3*N,3*N)); %space for padded image
subphot=uint8(255*ones(N,N)); %space for subsampled image

aphot(N+1:2*N,N+1:2*N)=phot(:,:); %photo padding

M=[1 1;-1 1]; %downsampling matrix

disp('please wait');

for nr=1:N,  %rows
   for nc=1:N,  %columns
      p=M*[nc;nr]; %select a sample
      subphot(nr,nc)=aphot(N+L+p(2),L+p(1));
   end;
end;

figure(1)
imshow(subphot);