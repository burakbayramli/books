% Modifiy HSV Parrot picture
parrot=imread('parrot1.jpg'); %read the image file into a matrix
parrothsv=rgb2hsv(parrot);

%change of Hue
aux=parrothsv(:,:,1); 
aux=aux+0.2; %hue shift
aux=mod(aux,1); %use reminder if >1
newparrot(:,:,1)=aux; %change of H

newparrot(:,:,2)=0.7*(parrothsv(:,:,2)); %change of S
newparrot(:,:,3)=0.9*(parrothsv(:,:,3)); %change of V

figure(1)
subplot(1,3,1);
imshow(newparrot(:,:,1)); 
title('(hue+0.2)mod1');

subplot(1,3,2);
imshow(newparrot(:,:,2)); 
title('0.7 x saturation');

subplot(1,3,3);
imshow(newparrot(:,:,3)); 
title('0.9 x value');


figure(2)
subplot(1,2,1)
imshow(parrot); %original
title('original');
h=gca;ht=get(h,'Title'); set(ht,'FontSize',14);

subplot(1,2,2)
newparrotrgb=hsv2rgb(newparrot); %to RGB
imshow(newparrotrgb);
title('modified');
h=gca;ht=get(h,'Title'); set(ht,'FontSize',14);

