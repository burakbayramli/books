% Blind Source Separation using ICA
% Example of two images

p1=imread('lily1.jpg'); %read the image file into a matrix
p2=imread('gioc1.jpg'); %read the image file into a matrix

N = 259*194; %picture size

%convert to vectors
s1=zeros(N,1); s2=zeros(N,1);
nn=1;
for ni=1:259,
   for nj=1:194,
      s1(nn)=p1(ni,nj);
      s2(nn)=p2(ni,nj);
      nn=nn+1;
   end;
end;  

%convert to +- 0.xxx float
ms1=mean(s1); ms2=mean(s2);
s1=s1-ms1; s1=s1/(max(abs(s1)));
s2=s2-ms2; s2=s2/(max(abs(s2)));
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

% Plot histogram of each source signal - 
% this approximates pdf of each source.
figure(1);
subplot(1,2,1); hist(s1,50); 
title('histogram of lily');
subplot(1,2,2); hist(s2,50); 
title('histogram of Gioconda'); 
drawnow;

s=[s1,s2]'; %combine sources

%mix of sources
M=[0.6 -0.4; -0.4 0.6]; %example of mixing matrix
x=M*s; %mixed signals

%mixed pictures
px1=zeros(259,194); nn=1;
for ni=1:259,
   for nj=1:194,
      px1(ni,nj)=x(1,nn);
      nn=nn+1;
   end;
end;

px2=zeros(259,194); nn=1;
for ni=1:259,
   for nj=1:194,
      px2(ni,nj)=x(2,nn);
      nn=nn+1;
   end;
end;

figure(2)
subplot(1,2,1); imshow(px1); 
title('mixed picture 1'); 
subplot(1,2,2); imshow(px2); 
title('mixed picture 2'); 
drawnow;

%initialization
W=eye(2,2); %unmixing matrix
y=W*x; %estimated sources

%prepare iterations
nit=60;
delta=0.4;
etpy=zeros(1,nit); %for entropies
grd=zeros(1,nit); %for gradients

disp('working...'); %ask for patience 

%iterations
for nn=1:nit,
   y=W*x; %estimated sources
   mhy=tanh(y); %for maximum entropy estimation
   deW = abs(det(W));
   h =((sum(sum(mhy)))/N) + (0.5*log(deW)); %entropy
   g = inv(W') - ((2*mhy*x')/N); %gradient matrix
   W=W+(delta*g); %update of the unmixing matrix
   etpy(nn)=h; grd(nn)=norm(g(:)); %save intermediate values
end;
   
%display
figure(3)
plot(etpy,'k'); hold on;
plot(grd,'r')
title('entropy, and norm of gradient matrix'); 
xlabel('number of iterations');

%separated pictures
py1=zeros(259,194); nn=1;
for ni=1:259,
   for nj=1:194,
      py1(ni,nj)=y(1,nn);
      nn=nn+1;
   end;
end; 

py2=zeros(259,194); nn=1;
for ni=1:259,
   for nj=1:194,
      py2(ni,nj)=y(2,nn);
      nn=nn+1;
   end;
end;

figure(4)
subplot(1,2,1)
imshow(py1);
title('recovered image 1');
subplot(1,2,2)
imshow(py2);
title('recovered image 2');

%print correlations between sources and estimations
cr=corrcoef([y' s']);
cr(3:4,1:2)


  