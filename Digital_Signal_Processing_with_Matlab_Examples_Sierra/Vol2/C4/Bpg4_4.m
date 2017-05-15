% Using x-y basis
x=-5:0.05:5;
y=-5:0.05:5;
N=length(x);

%horizontal
zx=zeros(N,N); %space for the function x
for ny=1:N,
   for nx=1:N,
      aux=(x(nx)^2)+(y(ny)^2);
      zx(ny,nx)=-x(nx)*exp(-aux/2);
   end;
end;

%vertical
zy=zeros(N,N); %space for the function y
for ny=1:N,
   for nx=1:N,
      aux=(x(nx)^2)+(y(ny)^2);
      zy(ny,nx)=-y(ny)*exp(-aux/2);
   end;
end;

%with 30º
za=zeros(N,N); %space for the function za
alpha=-pi/6;
ca=cos(alpha); sa=sin(alpha);
for ny=1:N,
   for nx=1:N,
      za(ny,nx)=ca*zx(ny,nx)+sa*zy(ny,nx);
   end;
end;


figure(1)
subplot(1,3,1);
mesh(x,y,zx);
view(0,90);
title('Horizontal derivative');
xlabel('X'); ylabel('Y');
subplot(1,3,2);
mesh(x,y,zy);
view(0,90);
title('Vertical derivative');
xlabel('X'); ylabel('Y');
subplot(1,3,3);
mesh(x,y,za);
view(0,90);
title('Synthesized angle');
xlabel('X'); ylabel('Y');

