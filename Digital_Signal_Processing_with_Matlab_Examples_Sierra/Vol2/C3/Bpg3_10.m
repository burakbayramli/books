% Display filtered gray scale picture
% log filter
keaton2=imread('keaton2bw.tif'); %read the image file into a matrix
fil=fspecial('log',[7 7],0.5); %filter molecule
fk=filter2(fil,keaton2);
bfk=uint8(round(abs(fk))); %convert to unsigned 8-bit
abfk=imadjust(bfk,[0.1 0.7],[]); %image adjust
figure(1)
subplot(1,2,1)
imshow(keaton2);
title('log filtering');
ylabel('original')
subplot(1,2,2)
imshow(abfk); %display the filtered image
ylabel('filtered')