%Example of picture denoising using Anisotropic TV
% and Split Bregman
%
lambda=0.2; mu=0.1;

P=imread('face1.jpg'); %read image
A=imnoise(P,'salt&pepper',0.01); %add salt&pepper noise

u=double(A); uo=u; un=u;
[ly,lx]=size(u);

%initialization of variables
lx1=lx-1; ly1=ly-1; 
dx=zeros(ly,lx); dy=zeros(ly,lx); 
bx=zeros(ly,lx); by=zeros(ly,lx);
s=zeros(ly,lx); as=zeros(ly,lx);
g=zeros(ly,lx);

KL=lambda/(mu+(4*lambda)); KM=mu/(mu+(4*lambda));

%main loop
for niter=1:3,
   
   %solve first equation with Gauss-Seidel
   for i=2:ly1,
      j=2:lx1;      
      un(i,j)=(KM*uo(i,j))+(KL*(u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)+...
         +dx(i-1,j)-dx(i,j)+dy(i,j-1)-dy(i,j)-...
         -bx(i-1,j)+bx(i,j)-by(i,j-1)+by(i,j)));
   end;
   u=un;
   
   %obtain the s  -------------------
   for i=2:ly1,
      j=2:lx1;
      s(i,j)=sqrt(abs((u(i+1,j)-u(i-1,j))/2+bx(i,j)).^2+...
         +abs((u(i,j+1)-u(i,j-1))/2+by(i,j)).^2);
   end;
   
   for i=1:ly1,
      j=1; s(i,j)=sqrt(abs((u(i+1,j)-u(i,j))+bx(i,j))^2+...
         +abs((u(i,j+1)-u(i,j))+by(i,j))^2);
   end;   
   for j=1:lx1,
      i=1; s(i,j)=sqrt(abs((u(i+1,j)-u(i,j))+bx(i,j))^2+...
         +abs((u(i,j+1)-u(i,j))+by(i,j))^2);
   end; 
   for i=2:ly,
      j=lx; s(i,j)=sqrt(abs((u(i,j)-u(i-1,j))+bx(i,j))^2+...
         +abs((u(i,j)-u(i,j-1))+by(i,j))^2);
   end; 
   for j=2:lx,
      i=ly; s(i,j)=sqrt(abs((u(i,j)-u(i-1,j))+bx(i,j))^2+...
         +abs((u(i,j)-u(i,j-1))+by(i,j))^2);
   end; 
   
   %obtain the dx, dy  -------------------
   ls=lambda*s; as=ls+1;
   for i=2:ly1,
    for j=1:lx,
       dx(i,j)=(ls(i,j).*((u(i+1,j)-u(i-1,j))/2+bx(i,j)))/as(i,j);
    end;   
   end; 
   for j=1:lx,
    i=1; dx(i,j)=(ls(i,j)*((u(i+1,j)-u(i,j))+bx(i,j)))/as(i,j);      
   end;  
   for j=1:lx,
    i=ly; dx(i,j)=(ls(i,j)*((u(i,j)-u(i-1,j))+bx(i,j)))/as(i,j);      
   end;  
   
   for i=1:ly,
    for j=2:lx1,
       dy(i,j)=(ls(i,j).*((u(i,j+1)-u(i,j-1))/2+by(i,j)))/as(i,j);  
    end;   
   end; 
   for i=1:ly,
    j=1; dy(i,j)=(ls(i,j)*((u(i,j+1)-u(i,j))+by(i,j)))/as(i,j);      
   end; 
   for i=1:ly,
    j=lx; dy(i,j)=(ls(i,j)*((u(i,j)-u(i,j-1))+by(i,j)))/as(i,j);      
   end; 
   
   %obtain the bx, by  -------------------
   for i=2:ly1,
      j=1:lx;
      bx(i,j)=bx(i,j)+((u(i+1,j)-u(i-1,j))/2-dx(i,j));
   end;
   for j=1:lx,
      i=1; bx(i,j)=bx(i,j)+((u(i+1,j)-u(i,j))-dx(i,j));      
   end;   
   for j=1:lx,
      i=ly; bx(i,j)=bx(i,j)+((u(i,j)-u(i-1,j))-dx(i,j));      
   end; 
   
   for i=1:ly,
      j=2:lx1;
      by(i,j)=by(i,j)+((u(i,j+1)-u(i,j-1))/2-dy(i,j));     
   end;
   for i=1:ly,
      j=1; by(i,j)=by(i,j)+((u(i,j+1)-u(i,j))-dy(i,j));      
   end;   
   for i=1:ly,
      j=lx; by(i,j)=by(i,j)+((u(i,j)-u(i,j-1))-dy(i,j));      
   end; 
   
end;   

%display
figure(1)
subplot(1,2,1)
imshow(A);
title('ROF-TV denoising using Split Bregman');
xlabel('original');
subplot(1,2,2)
imshow(uint8(un));
xlabel('denoised')