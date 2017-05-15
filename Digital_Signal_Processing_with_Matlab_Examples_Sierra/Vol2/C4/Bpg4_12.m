% Haar wavelet transform of a basic image
% recovery of image
% square and rhombus
% direct calculations: no filters
%
%----------------------------------------
% First the transformed images
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

%----------------------------------------
% Second the recovery of original image
% from LL,LH, HL and HH
%
rfg=zeros(256,256); %space for image
%
%recovery of L
for nn=1:128,
   auxL(1:2:256)=llfg1(1:128,nn)+hlfg1(1:128,nn);
   auxL(2:2:256)=llfg1(1:128,nn)-hlfg1(1:128,nn);
   lfg1(1:256,nn)=c*auxL; %image L   
end;
%recovery of H
for nn=1:128,
   auxH(1:2:256)=lhfg1(1:128,nn)+hhfg1(1:128,nn);
   auxH(2:2:256)=lhfg1(1:128,nn)-hhfg1(1:128,nn);
   hfg1(1:256,nn)=c*auxH; %image H   
end;

%recovery of original
for nn=1:256,
   auxL(1:2:256)=lfg1(nn,1:128)+hfg1(nn,1:128);
   auxL(2:2:256)=lfg1(nn,1:128)-hfg1(nn,1:128);
   rfg(nn,1:256)=c*auxL'; %image H   
end;

figure(1)
imshow(rfg);
title('recovered image');

