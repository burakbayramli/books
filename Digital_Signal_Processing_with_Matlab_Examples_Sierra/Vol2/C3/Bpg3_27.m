% Paint YIQ color space

%create an IQ color plane
nn=linspace(-1,1,200);
mm=fliplr(nn);
mY=ones(200,200);
mI=repmat(nn,[200,1]);
mQ=repmat(mm',[1,200]);

%prepare image
Iiq=zeros(200,200,3);
Iiq(:,:,1)=mY; Iiq(:,:,2)=mI; Iiq(:,:,3)=mQ;

%convert to RGB
Irgb=ntsc2rgb(Iiq);

%display
figure(1)
imshow(Irgb);
title('I-Q color space, Y=1');
xlabel('I'); ylabel('Q');