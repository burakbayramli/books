% Haar wavelet transform of a basic image
% recovery of image
% square and rhombus
% Using filters
%
%Haar filter
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[-c c]; %high-pass filter
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
lfg1=conv2(fg,h0); %low-pass filtering of rows
lfg1=lfg1(:,2:2:end); %downsampling to get L
hfg1=conv2(fg,h1); %high-pass filtering of rows
hfg1=hfg1(:,2:2:end); %downsampling to get H
%Step 2
%1 scale wavelet transform of columns of previous step
llfg1=conv2(lfg1,h0'); %low-pass filtering of L columns
llfg1=llfg1(2:2:end,:); %downsampling
hlfg1=conv2(lfg1,h1'); %high-pass filtering of L columns
hlfg1=hlfg1(2:2:end,:); %downsampling

lhfg1=conv2(hfg1,h0'); %low-pass filtering of H columns
lhfg1=lhfg1(2:2:end,:); %downsampling
hhfg1=conv2(hfg1,h1'); %high-pass filtering of H columns
hhfg1=hhfg1(2:2:end,:); %downsampling

figure(1)
imshow(fg);

%----------------------------------------
% Second the recovery of original image
% from LL,LH, HL and HH
%
rfg=zeros(256,256); %space for image
auxL=zeros(258,128);
auxH=zeros(258,128);
auxO=zeros(256,258);

%recovery of L
   auxL(1:2:258,:)=conv2(llfg1,h0')+conv2(hlfg1,h1');
   auxL(2:2:258,:)=conv2(llfg1,h0')-conv2(hlfg1,h1');
   lfg1=auxL(3:258,:);
%recovery of H 
   auxH(1:2:258,:)=conv2(lhfg1,h0')+conv2(hhfg1,h1');
   auxH(2:2:258,:)=conv2(lhfg1,h0')-conv2(hhfg1,h1');
   hfg1=auxH(3:258,:);

%recovery of original
   auxO(:,1:2:258)=conv2(lfg1,h0)+conv2(hfg1,h1);
   auxO(:,2:2:258)=conv2(lfg1,h0)-conv2(hfg1,h1);
   rfg=auxO(:,1:256)/4;
      
figure(2)
imshow(rfg);
title('recovered image');
h=gca;ht=get(h,'Title'); set(ht,'FontSize',12);

