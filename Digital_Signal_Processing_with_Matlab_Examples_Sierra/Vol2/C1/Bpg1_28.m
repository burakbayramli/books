% Watermarking via 2D DCT
%
P=imread('pengs.jpg'); %the cover
F=im2double(P);
[r,c]=size(F);

L=1024;
W=randn(1,L); %the watermark

DF=dct2(F); %2D DCT of the cover
vDF=reshape(DF,1,r*c); %convert to vector format
lv=length(vDF);

[v,ix]=sort(abs(vDF)); %vector reordering
cx=ix(lv-L:lv-1); %select L vector components (except DC entry)
%find which DF entries correspond to selected components
iDF=zeros(L,2);
for ne=1:L,
   ic=floor(cx(ne)/r)+1; %column
   ir=mod(cx(ne),r); %row
   iDF(ne,1)=ir; iDF(ne,2)=ic;
end;

%Insert the watermark
alpha=0.1; %parameter
DFW=DF;
for ne=1:L,
   ir=iDF(ne,1); ic=iDF(ne,2);
   DFW(ir,ic)=DFW(ir,ic)+ (alpha*DFW(ir,ic)*W(ne));
end;

%inverse 2D DCT
FW=idct2(DFW);

%extract the watermark
Wex=zeros(1,L);
for ne=1:L,
   ir=iDF(ne,1); ic=iDF(ne,2);
   Wex(ne)=((DFW(ir,ic)/DF(ir,ic))-1)/alpha;
end;

difW=W-Wex; %for checking

%display
figure(1)
imshow(F);
title('original image');

figure(2)
imshow(FW)
title('image with watermark');

figure(3)
plot(difW);
title('W-Wex');
axis([0 L -1e-14 1e-14]);



