% Display filtered gray scale picture
% unsharp filter
keaton=imread('keaton2bw.tif'); %read the image file into a matrix
fil=fspecial('unsharp'); %filter molecule
fk=filter2(fil,keaton);
bfk=uint8(round(abs(fk))); %convert to unsigned 8-bit
figure(1)
imshow(bfk); %display the filtered image
title('unsharp filtering');