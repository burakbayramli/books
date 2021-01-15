function [f]=obstc(nx,ny,f,uo,rho)

%length of obsticale= nx/5, and has sides of 10 units
nxb=(nx-1)/5;
nxe=nxb+10;
nyb=((ny-1)-10)/2;
nyb=35;
nye=nyb+10;
for i=nxb:nxe

f(i,nyb,4)=f(i,nyb,2);
f(i,nyb,7)=f(i,nyb,5);
f(i,nyb,8)=f(i,nyb,6);
f(i,nye,2)=f(i,nye,4);
f(i,nye,5)=f(i,nye,7);
f(i,nye,6)=f(i,nye,8);

end

%bottom, and top boundary, bounce back
for j=nyb:nye

f(nxb,j,3)=f(nxb,j,1);
f(nxb,j,7)=f(nxb,j,5);
f(nxb,j,6)=f(nxb,j,8);
f(nxe,j,1)=f(nxe,j,3);
f(nxe,j,5)=f(nxe,j,7);
f(nxe,j,8)=f(nxe,j,8);

end
for i=nxb:nxe

for j=nyb:nye

u(i,j)=0.0;
v(i,j)=0.0;

end

end

% End

end
