img_dist = imreadbw('img1.jpg');

K = [ 388.6795 0  343.7415;
       0  389.4250 234.6182;
       0 0 1];

KNew = [250 0.0 512;
        0 250 384;
        0.0 0.0 1.0];
    
    
    % TO IMPLEMENT

subplot(1,2,2)
imagesc(img_dist)
colormap gray
axis equal
subplot(1,2,1)
imagesc(img_undist)
colormap gray
axis equal

%%

img_dist = imreadbw('img2.jpg');

K = [279.7399 0 347.32012;
     0 279.7399 234.99819;
     0 0 1];

 

KNew = [200 0.0 512;
        0 200 384;
        0.0 0.0 1.0];
    

    % TO IMPLEMENT

subplot(1,2,2)
imagesc(img_dist)
colormap gray
axis equal
subplot(1,2,1)
imagesc(img_undist)
colormap gray
axis equal