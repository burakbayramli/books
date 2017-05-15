% Compression of the photograph using 2D DCT
P=imread('elef.jpg');
F=im2double(P);
M=dctmtx(8);
B=blkproc(F,[8 8],'P1*x*P2',M,M');
mask= [1  1  1  1  0  0  0  0
       1  1  1  0  0  0  0  0
       1  1  0  0  0  0  0  0
       1  0  0  0  0  0  0  0
       0  0  0  0  0  0  0  0
       0  0  0  0  0  0  0  0
       0  0  0  0  0  0  0  0
       0  0  0  0  0  0  0  0];
    
C=blkproc(B,[8 8],'P1.*x',mask);
RP=blkproc(C,[8 8], 'P1*x*P2',M',M); 

figure(1)
imagesc(C); colormap('prism');
title('the block by block transform');

figure(2)
imshow(RP);
title('recovered photo');
