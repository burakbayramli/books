% Horizontal derivative of Gaussian
x=-6:0.1:6;
y=-6:0.1:6;
N=length(x);

z=zeros(N,N); %space for the function
for ny=1:N,
   for nx=1:N,
      aux=(x(nx)^2)+(y(ny)^2);
      z(ny,nx)=-x(nx)*exp(-aux/2);
   end;
end;

figure(1)
mesh(x,y,z);
view(20,30);
title('Horizontal derivative of Gaussian');
xlabel('X'); ylabel('Y');
