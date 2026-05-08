function [f]=obstc(nx,ny,f,uo,rho)

%length of obsticale= nx/4, heigh ny/2
nxo=(nx-1)/4;
nyl=(ny-1)/2;
for i=1:nxo

f(i,nyl,2)=f(i,nyl,4);
f(i,nyl,5)=f(i,nyl,7);
f(i,nyl,6)=f(i,nyl,8);

end

%bottom, and top boundary, bounce back
for j=1:nyl

f(nxo,j,1)=f(nxo,j,3);


f(nxo,j,5)=f(nxo,j,7);
f(nxo,j,8)=f(nxo,j,8);

end
for i=1:nxo

for j=1:nyl

u(i,j)=0.0;
v(i,j)=0.0;

end

end

% End

end
