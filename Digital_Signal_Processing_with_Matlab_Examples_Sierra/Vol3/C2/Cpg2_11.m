%Example of heat diffusion
%
%Initialization of variables
D=0.25; %diffusion constant
dx=1; dy=dx; %we will use a regular grid, with dy=dx
dt=1; 
x=0:dx:99; y=0:dy:99; %the grid
lx=length(x); ly=length(y);
u=zeros(lx,ly); un=u;

u(46:54,46:54)=1000; %central region is heated

%first display
figure(1)
subplot(2,3,1);
mesh(u);
axis([0 100 0 100 0 1000]);
title('heat diffusion');

for nn=2:6,
   
  for T=1:dt:30, %time
   for i=2:ly-1,
      for j=2:lx-1,
         aux=u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)-(4*u(i,j));
         un(i,j)=u(i,j)+((D*dt*aux)/dx^2);
      end;
   end; 
   u=un;
  end

subplot(2,3,nn); %the other plots
mesh(un); 
axis([0 100 0 100 0 500]);
end;