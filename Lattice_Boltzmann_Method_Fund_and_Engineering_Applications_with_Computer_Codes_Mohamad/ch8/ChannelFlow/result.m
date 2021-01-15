function result(nx,ny,x,y,u,v,uo,rho)
for j=1:ny

Tm1(j)=u(101,j)/uo;
Tm2(j)=u(201,j)/uo;
Tm3(j)=u(301,j)/uo;
Tm4(j)=u(401,j)/uo;
Tm5(j)=u(601,j)/uo;

end

figure

plot(Tm1,y,Tm2,y,Tm3,y,Tm4,y,Tm5,y,'LineWidth',1.5)

xlabel('U')
ylabel('Y')
%Stream function calculation
for j=1:ny

sx(:,j)=x(:);

end
for i=1:nx

sy(i,:)=y(:);

end

str=zeros(nx,ny);
for i=1:nx
for j=2:ny

str(i,j)=str(i,j-1)+0.5*(u(i,j)+u(i,j-1));

end
end
figure
contour(sx,sy,str)

end
