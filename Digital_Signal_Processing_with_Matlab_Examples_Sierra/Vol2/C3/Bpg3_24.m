% Paint RGB gamut in the CIE color space

%The CIE perimeter data
datxyz=load('cie1.txt'); %load cie table
[N,l]=size(datxyz);
nC=zeros(N,3); %reserve space
C=datxyz(:,2:4); %XYZ data
for nn=1:N,
   nC(nn,:)=C(nn,:)/sum(C(nn,:)); %normalize: xyz data
end;

%The XYZ to RGB conversion matrix
C2R=[1.7552599 -0.4836786 -0.2530;
     -0.5441336 1.5068789 0.0215528;
     0.0063467 -0.0175761 1.2256959];
  
%gamut computation 
G=zeros(100,100,3);
for ni=1:100,
   for nj=1:100,
      x=nj/100; y=ni/100; z=1-x-y;
      G(ni,nj,:)=C2R*[x y z]';
      % negative or >1 values set to white
      m=min(G(ni,nj,:)); M=max(G(ni,nj,:));
      if (m<0 | M>1), G(ni,nj,:)=ones(1,3); end; %white
   end;
end;   

figure(1)
Cx=100*nC(:,1); Cy=100*nC(:,2);
imshow(G);
line([Cx' Cx(1)],[Cy' Cy(1)],'LineWidth',2);
axis square; 
axis xy; 
axis on;
title('RGB gamut');


