% Haar wavelet transform of a basic image
% square and rhombus
% direct calculations: no filters
%
%Original image
fg=ones(256,256); %white plane
%the rhombus
for n=1:32,
   fgx=64+(-n:n); fgy=96+n;
   fg(fgx,fgy)=0; %one triangle
   fg(fgx,256-fgy)=0; %the other triangle
end
%the square
fg(128:196,96:160)=0;
%
figure(1)
imshow(fg); %plots the binary image
title('Haar wavelet transform example');
ylabel('original');
%
%Step1
%1 scale wavelet transform of rows
c=1/sqrt(2);
lfg1=zeros(256,128); %space for image
hfg1=zeros(256,128); %"   "
for nn=1:256,
   auxL=fg(nn,1:2:256)+fg(nn,2:2:256);
   auxH=fg(nn,1:2:256)-fg(nn,2:2:256);
   lfg1(nn,1:128)=c*auxL; %image L
   hfg1(nn,1:128)=c*auxH; %image H
end;

%Step 2
%1 scale wavelet transform of columns of previous step
llfg1=zeros(128,128); %space for image
hlfg1=zeros(128,128); %"   "
hhfg1=zeros(128,128); %space for image
lhfg1=zeros(128,128); %"   "
%columns of L
for nn=1:128,
   auxL=lfg1(1:2:256,nn)+lfg1(2:2:256,nn);
   auxH=lfg1(1:2:256,nn)-lfg1(2:2:256,nn);
   llfg1(1:128,nn)=c*auxL; %image LL
   hlfg1(1:128,nn)=c*auxH; %image HL
end;
%columns of H
for nn=1:128,
   auxL=hfg1(1:2:256,nn)+hfg1(2:2:256,nn);
   auxH=hfg1(1:2:256,nn)-hfg1(2:2:256,nn);
   lhfg1(1:128,nn)=c*auxL; %image LH
   hhfg1(1:128,nn)=c*auxH; %image HH
end;

%normalization to bw
A=zeros(128,128);
Nllfg1=A; Nlhfg1=A; Nhlfg1=A; Nhhfg1=A;
for nn=1:128,
   for mm=1:128,
      if abs(llfg1(nn,mm))>0, Nllfg1(nn,mm)=1; end;
      if abs(lhfg1(nn,mm))>0, Nlhfg1(nn,mm)=1; end;
      if abs(hlfg1(nn,mm))>0, Nhlfg1(nn,mm)=1; end;
      if abs(hhfg1(nn,mm))>0, Nhhfg1(nn,mm)=1; end;
   end;
end;

%Figures
figure(2)
subplot(2,2,1)
imshow(Nllfg1); %LL image
title('LL');
subplot(2,2,2)
imshow(Nlhfg1); %LH image
title('HL');
subplot(2,2,3)
imshow(Nhlfg1); %HL image 
title('LH');
subplot(2,2,4)
imshow(Nhhfg1); %HH image
title('HH');


