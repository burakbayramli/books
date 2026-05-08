% Collition
function [f]=collition(nx,ny,u,v,cx,cy,omega,f,rho,w)
for j=1:ny

for i=1:nx

t1=u(i,j)*u(i,j)+v(i,j)*v(i,j);
for k=1:9

t2=u(i,j)*cx(k)+v(i,j)*cy(k);

feq(i,j,k)=rho(i,j)*w(k)*(1.0+3.0*t2+4.5*t2*t2-1.5*t1);
f(i,j,k)=(1.-omega)*f(i,j,k)+omega*feq(i,j,k);

end

end

end
end
