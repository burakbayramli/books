% Watermarking via image bits
%
P=imread('pengs.jpg'); %the cover (256x256 image)
F=im2uint8(P);

S=imread('sign1.jpg'); %the message (256x256 binary image)
M=im2uint8(S>30); %threshold

%embed watermark
for nr=1:256,
for nc=1:256,
   aux=bitget(M(nr,nc),1); %read bit of message
   Q(nr,nc)=bitset(F(nr,nc),1,aux); %apply that bit to LSB of cover
end;
end;

%extract watermark
W=zeros(256,256);
for nr=1:256,
for nc=1:256,
   aux=bitget(Q(nr,nc),1); %read LSB of watermarked image
   if aux==1, W(nr,nc)=256; end; %recover message
end;
end;

%display
figure(1)
imshow(Q);
title('image with watermark');

figure(2)
imshow(W*256);
title('the hidden message');
