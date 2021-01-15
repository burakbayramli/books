function result(nx,ny,x,y,u,v,uo,rho,rhog)
for j=1:ny

for i=1:nx

xp(i,j)=x(i);
yp(i,j)=y(j);

end

end
for j=1:ny

uvm(j)=u((nx-1)/2,j)/uo;
tmj(j)=rhog((nx-1)/2,j);

end

for i=1:nx
vvm(i)=v(i,(ny-1)/2)/uo;
end
figure
plot(tmj,y,'LineWidth',2)

figure

plot(uvm,y,'LineWidth',2)

xlabel('U')
ylabel('Y')

figure

plot(x,vvm,'LineWidth',1.5)
xlabel('X')
ylabel('V')

figure
quiver(x,y,v,u, 10)
%Stream function calculation
str=zeros(nx,ny);
for i=1:nx
for j=2:ny

str(i,j)=str(i,j-1)+0.25*(rho(i,j)+rho(i,j-1))*(u(i,j)+u(i,j-1));

end
end

figure
contour(x,y,str,20)
%or use the following build-in function
figure
startx=0.0:0.1:1;
starty=0.0:0.1:1;
streamline(x,y,v,u,startx,starty)
figure
contour(xp,yp,rhog,20)
end
