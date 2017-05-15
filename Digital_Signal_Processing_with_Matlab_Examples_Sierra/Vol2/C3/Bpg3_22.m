% Display B&W thresholded picture
cells=imread('cells1bw.tif'); %read the image file into a matrix
figure(1)
subplot(1,2,1)
imshow(cells); %display original
title('image threholding');
ylabel('original')
subplot(1,2,2)
imshow(cells>180); %display thresholded image
ylabel('thresholded');
