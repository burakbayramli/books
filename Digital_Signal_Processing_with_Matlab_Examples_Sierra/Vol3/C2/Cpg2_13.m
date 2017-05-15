%Example of picture anti-diffusion sharpening
% (reverse heat diffusion)
%
%Initialization of variables
D=0.05; %diffusion constant
dx=1; %we will use a regular grid, with dy=dx
dt=1; 
x=0:dx:399; y=0:dy:249; %the grid (image size)
lx=length(x); ly=length(y);

P=imread('spencer.jpg'); %read image
u=double(P);
un=u;

%first display
figure(1)
subplot(2,1,1);
imshow(uint8(u));
title('Gaussian anti-diffusion');
 
  for T=1:dt:5, %time
   for i=2:ly-1,
      for j=2:lx-1,
         aux=u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)-(4*u(i,j));
         un(i,j)=u(i,j)-((D*dt*aux)/dx^2);
      end;
   end; 
   u=un;
  end

subplot(2,1,2); %the other plot
imshow(uint8(un)); 
