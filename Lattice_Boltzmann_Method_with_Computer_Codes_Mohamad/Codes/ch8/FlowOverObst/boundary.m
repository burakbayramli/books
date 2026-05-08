%Boudary conditions for Channel flow
function [f]=boundary(nx,ny,f,uo,rho)

%right hand boundary

for j=1:ny

f(nx,j,3)=f(nx-1,j,3);
f(nx,j,7)=f(nx-1,j,7);
f(nx,j,6)=f(nx-1,j,6);

end

%bottom, and top boundary, bounce back
for i=1:nx

f(i,1,2)=f(i,1,4);
f(i,1,5)=f(i,1,7);
f(i,1,6)=f(i,1,8);
f(i,ny,4)=f(i,ny,2);
f(i,ny,7)=f(i,ny,5);
f(i,ny,8)=f(i,ny,6);
u(i,1)=0.0; v(i,1)=0.0;
u(i,ny)=0.0; v(i,ny)=0.0;

end
%Left boundary, velocity is given= uo

for j=2:ny-1
f(1,j,1)=f(1,j,3)+2.*rho(1,j)*uo/3.;
f(1,j,5)=f(1,j,7)-0.5*(f(1,j,2)-f(1,j,4))+rho(1,j)*uo/6.;
f(1,j,8)=f(1,j,6)+0.5*(f(1,j,2)-f(1,j,4))+rho(1,j)*uo/6.;
u(1,j)=uo; v(1,j)=0.0;
end

% End of boundary conditions.

end
