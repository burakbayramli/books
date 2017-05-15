% Edges of B&W picture
zebra=imread('zebra1bw.tif'); %read the image file into a matrix
[ly,lx]=size(zebra);
EZb=zeros(ly,lx,1,6);

figure(1)
imshow(zebra); %original  picture
title('original picture');

figure(2)
EZb(:,:,1,1)=edge(zebra,'prewitt'); %Prewitt method
EZb(:,:,1,2)=edge(zebra,'roberts'); %Roberts method
EZb(:,:,1,3)=edge(zebra,'sobel'); %Sobel method
EZb(:,:,1,4)=edge(zebra,'log'); %Laplacian or Gaussian method
EZb(:,:,1,5)=edge(zebra,'zerocross'); %zero crossings method
EZb(:,:,1,6)=edge(zebra,'canny'); %Canny method

montage(EZb); %shows the 6 images
title('edge computation alternatives');

figure(3)
imshow(EZb(:,:,1,6)); %one of the results
title('result of the Canny method');
