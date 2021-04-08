function b=assemblyfluidvector(Nx,Ny,x0,xf,y0,yf,Re)
vnd=(Nx+1)*(Ny+1);
vnd=(Nx+1)*(Ny+1);
mnd=(Nx+1)*Ny+Nx*(Ny+1);
nd=vnd+mnd;
neq=2*nd+vnd;
celem=connective(Nx,Ny);
elem4=[celem(:,9:12)];
elem8=[celem(:,1:8)];
node=node48(Nx,Ny,x0,xf,y0,yf);
n=2*length(celem(1,:))-4;
ne=length(celem(:,1));
gdof=fungdof(Nx,Ny,x0,xf,y0,yf);
b=zeros(neq,1);
for e=1:ne
    belem=localfluidvector(celem);
    for i = 1:n
        b(gdof(e,i))=b(gdof(e,i))+belem(i);
    end
end
