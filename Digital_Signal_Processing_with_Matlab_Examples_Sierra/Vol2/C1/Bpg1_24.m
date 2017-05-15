% 2D DCT of a photograph
P=imread('elef.jpg');
D=dct2(P);

figure(1)
imshow(P);
title('original photo');

figure(2)
imshow(log(abs(D)));
title('2D DCT of the photo');
