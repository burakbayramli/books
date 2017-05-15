% Image downsampling (causing shearing) example
% 
phot=imread('London1.tif'); %read the 256x256 image file into a matrix

N=256;

aphot=uint8(255*ones(N,3*N)); %space for padded image
subphot=uint8(255*ones(N,2*N)); %space for sheared image

aphot(1:N,N+1:2*N)=phot(:,:); %photo padding

M=[1 1;0 1]; %downsampling matrix

disp('please wait');

for nr=1:N,  %rows
   for nc=1:2*N,  %columns
      p=M*[nc;nr]; %select a sample
      subphot(nr,nc)=aphot(p(2),p(1));
   end;
end;

figure(1)
imshow(subphot);