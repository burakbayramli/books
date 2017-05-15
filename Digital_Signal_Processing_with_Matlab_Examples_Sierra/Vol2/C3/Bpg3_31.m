% Convert Parrot picture to HSV
parrot=imread('parrot1.jpg'); %read the image file into a matrix
parrothsv=rgb2hsv(parrot);
figure(1)
subplot(1,3,1);
imshow(parrothsv(:,:,1)); 
title('hue');
subplot(1,3,2);
imshow(parrothsv(:,:,2)); 
title('saturation');
subplot(1,3,3);
imshow(parrothsv(:,:,3)); 
title('value');
