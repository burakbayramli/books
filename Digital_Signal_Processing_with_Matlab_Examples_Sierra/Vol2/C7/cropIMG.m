% Display of 1 plane picture
keaton=imread('gioc1.jpg'); %read the image file into a matrix
figure(1)
imshow(keaton); pixval on; %display the photo
figure(2)
nk=keaton(1:259,1:194);
imshow(nk);
imwrite(nk,'c:\aaa\gioc2.jpg','jpg');