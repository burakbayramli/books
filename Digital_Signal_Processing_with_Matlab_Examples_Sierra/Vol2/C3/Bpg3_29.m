% Display RGB planes of Parrot picture
parrot=imread('parrot1.jpg'); %read the image file into a matrix
figure(1)
imshow(parrot); %display color picture
title('original');
figure(2)
subplot(1,3,1);
imshow(parrot(:,:,1)); %display R
title('R');
subplot(1,3,2);
imshow(parrot(:,:,2)); %display G
title('G');
subplot(1,3,3);
imshow(parrot(:,:,3)); %display B
title('B');

