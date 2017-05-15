%Ensure black color
NN=512;
ckei=kei;
kei=imread('Bmg1.tif'); %read the image file into a matrix
for ni=1:NN,
for nj=1:NN,
   if kei(ni,nj)<50,ckei(ni,nj)=0; end; %Threshold
end;
end;
      
figure(1)
imshow(ckei); pixval on; %display the photo
imwrite(ckei,'c:\aaa\Bmg1a.tif','tif');