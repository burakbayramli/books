% Color reduction using K-means

F=imread('houses1','jpg');
[hF,wF,cF]=size(F);
L=zeros(hF,wF); %reserve for labels assigned to pixels

% show original painting
figure(1)
imshow(F,[]);
title('original picture');

% initial centroids
C=zeros(3,16);
C(:,1)=[0.9;0.9;0.9]; C(:,2)=[0.1;0.1;0.1]; C(:,3)=[0.8;0.9;0.3];
C(:,4)=[0.6;0.7;0.6]; C(:,5)=[0.8;0.8;0.8]; C(:,6)=[0.9;0.7;0.7];
C(:,7)=[0.4;0.4;0.5]; C(:,8)=[0.5;0.3;0.2]; C(:,9)=[0.5;0.5;0.2];
C(:,10)=[0.7;0.5;0.5]; C(:,11)=[0.5;0.7;0.5]; C(:,12)=[0.5;0.6;0.6];
C(:,13)=[0.6;0.6;0.6]; C(:,14)=[0.8;0.8;0.95]; C(:,15)=[0.5;0.6;0.3];
C(:,16)=[0.3;0.3;0.3];

SD=zeros(3,16); % for adding distances
ND=zeros(1,16); % counter of set members for each centroid

% show initial palette---------------------------------------------
[PL]=Palette(C);
figure(2);imshow(PL);
title('initial palette');

disp('wait for next 10 iterations');
%========================================================
% K-means algorithm
for it=1:10,

%--------------------------------------------------------
% assign centroids to image pixels
D=zeros(1,16);
for nl=1:hF,
   for nc=1:wF,
       aux1=im2double(F(nl,nc,:)); aux2=squeeze(aux1);
       for mm=1:16, 
          D(mm)=norm(aux2-C(:,mm)); %see distance to centroid mm
       end;
       [dist,imin]=min(D); % select centroid with shortest distance
       L(nl,nc)=imin; % assign label to pixel
       SD(:,imin)=SD(:,imin)+aux2; % add values of pixel
       ND(imin)=ND(imin)+1; % increase members counter
   end;
end;
disp('***');

%--------------------------------------------------------
% move centroids
for nn=1:16,
   aux=ND(nn);
   if aux>0,
      avg=SD(:,nn)/aux;
      C(:,nn)=avg;
   end;   
end;   

%--------------------------------------------------------
end;

disp('the end')

% form new palette---------------------------------------------
[PL]=Palette(C);

% display re-colored image
RF=im2double(F); %init
for nl=1:hF,
   for nc=1:wF,      
      mm=L(nl,nc); % retrieve centroid number
      aux=C(:,mm);
      RF(nl,nc,:)=aux;
   end;
end;

%-------------------------------------------------------
% show the results of K-means
figure(3)
% show the palette found by the algorithm
imshow(PL);
title('Palette found by K-means');

figure(4)
% show re-colored image
imshow(RF);
title('re-colored picture');