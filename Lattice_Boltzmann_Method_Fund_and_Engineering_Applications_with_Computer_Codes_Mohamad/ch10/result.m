function result(nx,ny,x,y,u,v,uo,rho)
for j=1:ny

um(j)=u((nx-1)/2,j)/uo;

end

for i=1:nx
vm(i)=v(i,(ny-1)/2)/uo;
end

figure

line(um,y,'LineWidth',1.5)

xlabel('U')


ylabel('Y')

figure

line(x,vm,'LineWidth',1.5)
xlabel('X')
ylabel('V')

figure
quiver(x,y,v/uo,u/uo, 10)
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
end
