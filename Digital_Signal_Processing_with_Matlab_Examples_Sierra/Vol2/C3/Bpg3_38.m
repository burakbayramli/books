% Hough photo projection example
clear all;

phot=imread('Viet1.jpg'); %read the image file into a matrix
phot1=phot(:,:,1);
pho=phot1(40:430,70:590); %limit the size
A=edge(pho,'canny'); %get edges

%prepare for Hough transform
[x,y]=find(A); %indices of non-zero pixels
Nx=length(x); %number of points

%compute projections
ang=([-90:180]*pi)/180; %set of angles
Na=length(ang); %number of angles
P=floor(x*cos(ang)+y*sin(ang)); %integer projections
Pmax=max(max(P)); %maximum P

%accumulate
AC=zeros(Pmax+1,Na); %accumulator
for nj=1:Na, %angles
for ni=1:Nx,  %points
   aux=P(ni,nj);
   if aux>=0,
      AC(1+aux,nj)=1+AC(1+aux,nj);
   end;
end;
end;

figure(1)
imagesc(AC);
axis([0 Na 0 Pmax+1]);
h=gca; set(h,'YDir','normal');
title('Hough transform: sinogram');
xlabel('angles in degrees'); ylabel('projection');

%-----------------------------------
% plot detected lines on photograph
M=max(max(AC)); %accumulator maximum
[dP,dphi]=find((AC<=M-44) & (AC>M-46)); 
Lm=length(dP); %number of maximum fits
dang=((181-dphi)*pi)/180; %from degrees to radians

figure(2)
imshow(pho); hold on; %the photo
title('original photo');

figure(3)
imshow(pho); hold on; %the photo
title('image and detected lines');
%add lines
[X,Y]=size(pho);

for nn=1:Lm,
   S=sin(dang(nn)); C=cos(dang(nn));
   rr=dP(nn);
   if S==0,
      line([rr,rr], [0,Y], 'Color','Cyan');
   else
      yrr=(rr-(Y*C))/S;
      line([0,Y], [rr/S,yrr],'Color','Yellow');
   end;
end   
  
