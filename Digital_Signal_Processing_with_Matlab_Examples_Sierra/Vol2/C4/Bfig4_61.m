% Extract a square from image
tfg=B_2D_Haar; %function call
rbeg=1; Nr=size(tfg,1);
cbeg=5*Nr/8;
cw=(Nr/8)-1; 
sq=tfg(rbeg:rbeg+cw,cbeg:cbeg+cw);
imshow(sq);

