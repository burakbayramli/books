function [forcx,forcy]=force(nx,ny,u,v,cx,cy,rho,w,G)
for j=1:ny

for i=1:nx

forcxs=0.0;
forcys=0.0;
for k=1:8

newx=1+mod(i-1+cx(k)+nx,nx);


newy=1+mod(j-1+cy(k)+ny,ny);
psi=1-exp(-rho(newx,newy));
forcxs=forcxs-G*w(k)*psi*cx(k);
forcys=forcys-G*w(k)*psi*cy(k);

end
forcx(i,j)=(1-exp(-rho(i,j)))*forcxs;
forcy(i,j)=(1-exp(-rho(i,j)))*forcys;

end

end

end
