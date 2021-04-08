function A=assemblymatrix(Nx,Ny,x0,xf,y0,yf,u0,v0,Re)
vnd=(Nx+1)*(Ny+1);
mnd=(Nx+1)*Ny+Nx*(Ny+1);
nd=vnd+mnd; % velocity nodes
neq=2*nd+vnd; % total number of degrees of freedom
celem=connective(Nx,Ny);
elem4=[celem(:,9:12)];
elem8=[celem(:,1:8)];
node=node48(Nx,Ny,x0,xf,y0,yf);
gdof=fungdof(Nx,Ny,x0,xf,y0,yf);
n=2*length(celem(1,:))-length(elem4(1,:));
ne=length(celem(:,1)); %number of elements
A=zeros(neq,neq);
%invel=zeros(neq,1);
%invel(1:nd)=1;
%v0=invel(nd+vnd+1:neq);
for e=1:ne
    Aelem=fluidelemmatrix(node(elem8(e,:),:),...
        node(elem4(e,:),:),u0(elem8(e,:),:),v0(elem8(e,:),:),Re);
    for i=1:n
        for j=1:n
            A(gdof(e,i),gdof(e,j))=A(gdof(e,i),gdof(e,j))+Aelem(i,j);
        end
    end
end
