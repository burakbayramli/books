% Collition
function [f]=collision(nx,ny,uf,vf,cx,cy,omega,f,rho,w,forcx,forcy)
for j=1:ny

for i=1:nx

t1=uf(i,j)*uf(i,j)+vf(i,j)*vf(i,j);
for k=1:9

t2=uf(i,j)*cx(k)+vf(i,j)*cy(k);

feq(i,j,k)=rho(i,j)*w(k)*(1.0+3.0*t2+4.5*t2*t2-1.5*t1);
ffx=(3.*(cx(k)-uf(i,j))+9.*cx(k)*t2)*forcx(i,j);
ffy=(3.*(cy(k)-vf(i,j))+9.*cy(k)*t2)*forcy(i,j);
fftot=w(k)*(1.-0.5*omega)*(ffx+ffy);
f(i,j,k)=(1.-omega)*f(i,j,k)+omega*feq(i,j,k)+fftot;

end

end

end
end
