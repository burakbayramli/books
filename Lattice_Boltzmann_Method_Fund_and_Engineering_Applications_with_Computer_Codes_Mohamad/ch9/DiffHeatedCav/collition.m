% Collition
function [f]=collition(nx,ny,u,v,cx,cy,omega,f,rho,w,rhog,gbeta)
for j=1:ny

for i=1:nx

t1=u(i,j)*u(i,j)+v(i,j)*v(i,j);
for k=1:9

force=3.*w(k)*gbeta*rhog(i,j)*rho(i,j)*cy(k);
if(i==1 || i==nx) force=0.0;
end
if(j==1 || j==ny) force =0.0;
end
t2=u(i,j)*cx(k)+v(i,j)*cy(k);

feq=rho(i,j)*w(k)*(1.0+3.0*t2+4.5*t2*t2-1.5*t1);
f(i,j,k)=(1.-omega)*f(i,j,k)+omega*feq+force;

end

end

end
end
