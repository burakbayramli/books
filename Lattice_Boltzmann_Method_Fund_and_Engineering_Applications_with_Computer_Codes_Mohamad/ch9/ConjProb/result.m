function result(nx,ny,x,y,u,v,uo,rho,rhog)
%solid region velocity decoration

for j=1:11


for i=1:nx

u(i,j)=0.0;
v(i,j)=0.0;
u(i,ny-j)=0.0;
v(i,ny-j)=0.0;

end

end

for j=1:ny

for i=1:nx

xp(i,j)=x(i);
yp(i,j)=y(j);

end

end for j=1:ny

uvm(j)=u((nx-1)/2,j)/uo;
tmj(j)=rhog((nx-1)/2,j);
ttj(j)=rhog(nx-40,j);

end

figure

plot(tmj,y,ttj,y,'LineWidth',2)

figure

plot(uvm,y,'LineWidth',2)

xlabel('U')
ylabel('Y')

%Stream function calculation
str=zeros(nx,ny);
for i=1:nx
for j=2:ny

str(i,j)=str(i,j-1)+0.25*(rho(i,j)+rho(i,j-1))*(u(i,j)+u(i,j-1));

end
end
figure
contour(xp,yp,u,20)

%or use the following build-in function
figure
contour(xp,yp,rhog,20)

end
