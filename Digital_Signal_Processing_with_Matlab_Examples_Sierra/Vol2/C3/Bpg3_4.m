% Adjust gray scale picture
keaton=imread('keaton1bw.tif'); %read the image file into a matrix
adk=imadjust(keaton,[0.1 0.8],[]); %image adjust
figure(1)
subplot(1,2,1)
imshow(keaton); %plots original picture
title('image adjust');
ylabel('original');
subplot(1,2,2)
imshow(adk); %plots adjusted picture
ylabel('adjusted');
