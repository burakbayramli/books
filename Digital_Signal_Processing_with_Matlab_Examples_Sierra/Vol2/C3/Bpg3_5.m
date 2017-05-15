% Display filtered gray scale picture
% average filter
keaton2=imread('keaton2bw.tif'); %read the image file into a matrix
fil=fspecial('average',[5,5]); %filter molecule
fk=filter2(fil,keaton2);
bfk=uint8(round(fk)); %convert to unsigned 8-bit
figure(1)
subplot(1,2,1)
imshow(keaton2);
title('average filtering');
ylabel('original')
subplot(1,2,2)
imshow(bfk); %display the filtered image
ylabel('filtered')