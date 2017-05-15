% Histogram equalization of gray scale picture
keaton=imread('keaton1bw.tif'); %read the image file into a matrix
hqk=histeq(keaton); %histogram equalization
figure(1)
subplot(2,2,1)
imshow(keaton); %plots original picture
title('histogram equalization');
ylabel('original');
subplot(2,2,2)
imshow(hqk); %plots equalized picture
ylabel('equalized');
subplot(2,2,3)
imhist(keaton); %plots histogram original picture
ylabel('original');
subplot(2,2,4)
imhist(hqk); %plots histogram equalized picture
ylabel('equalized');