% Paint YUV color space

%YUV to RGB conversion matrix
U2R=[298 0 409;
    298 -100 -208;
    298 516 0];
U2R=U2R/256;

Y=230; %maximum
C=Y-16;
Irgb=zeros(256,256,3);
for nV=1:256,
   V=nV-1;
   for nU=1:256,
      U=nU-1;
      D=U-128;
      E=V-128;
      aux0=[C;D;E]; %data vector
      offs=[0.5;0.5;0.5]; %offset
      aux1=(U2R*aux0)+offs; %convert pixel to RGB
      aux1=uint8(aux1); %clip values into 0..255
      Irgb(257-nV,nU,:)=aux1; %put pixel in image     
   end;
end;

Irgb=Irgb/256; %0..1 values range

%display
figure (1)
imshow(Irgb);
title('U-V color space, Y=230');
xlabel('U'); ylabel('V');

