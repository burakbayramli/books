function bdof=funbdof(Nx,Ny)
nb=10*(Nx+Ny);
bdof=zeros(nb,1);
for k=1:2*Nx+2
    bdof(k)=k;
end
for k=(2*Nx+3):2:(2*Nx+3)+4*(Ny-1)
    if mod(Nx,2)==1
        if mod((k-1)/2,2)==0
            bdof(k)=bdof(k-2)+Nx+1;
        else
            bdof(k)=bdof(k-2)+2*Nx+1;
        end
        bdof(k+1)=bdof(k)+1;
    else
        if mod((k-1)/2,2)==1
            bdof(k)=bdof(k-2)+Nx+1;
        else
            bdof(k)=bdof(k-2)+2*Nx+1;
        end
        bdof(k+1)=bdof(k)+1;
    end
end
for k=2*Nx+1+4*Ny-2:4*(Nx+Ny)
    bdof(k)=Ny*(3*Nx+2)-(2*Nx+1+4*Ny-2)+k;
end

for k=4*(Nx+Ny)+1:4*(Nx+Ny)+Nx+2
    bdof(k)=bdof(k-1)+1;
end
for k=(4*(Nx+Ny)+Nx+2+1):2:(4*(Nx+Ny)+Nx+2+1+2*(Ny-2))
    bdof(k)=bdof(k-2)+Nx+1;
end

for k=(4*(Nx+Ny)+Nx+2+2):2:(4*(Nx+Ny)+Nx+2+1+2*(Ny-2))
    bdof(k)=bdof(k-1)+1;
end

for k=((4*(Nx+Ny)+Nx+2+1+2*(Ny-2))+1):((4*(Nx+Ny)+Nx+2+1+2*(Ny-2))+1)+Nx
    bdof(k)=bdof(k-1)+1;
end
bdof(((4*(Nx+Ny)+Nx+2+1+2*(Ny-2))+1)+Nx+1:10*(Nx+Ny))=bdof(1:4*(Nx+Ny))+...
    bdof(((4*(Nx+Ny)+Nx+2+1+2*(Ny-2))+1)+Nx);