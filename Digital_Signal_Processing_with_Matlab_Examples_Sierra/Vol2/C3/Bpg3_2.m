% Histogram of gray scale picture
keaton=imread('keaton1bw.tif'); %read the image file into a matrix
figure(1)
imhist(keaton); %plots histogram
title('image histogram');
