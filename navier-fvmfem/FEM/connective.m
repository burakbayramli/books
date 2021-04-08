function celem=connective(Nx,Ny)
%Nx=Number of elements in x-direction
%Ny=Number of elements in y-direction
%elem4=4 noded elements with global nodes
%elem8=8 noded elements with global nodes
elem4=zeros(Nx*Ny,4);
for i=1:Nx*Ny
    r(i)=mod(i,Nx);
    q(i)=(i-r(i))/(Nx);
    if r(i)==0;
        s(i)=i+q(i)-1;
    else
        s(i)=i+q(i);
    end
end
for k=1:Nx
    elem4(k,1)=2*k-1;
    elem4(k,2)=2*k+1;
    elem4(k,3)=elem4(k,2)+3*Nx+2;
    elem4(k,4)=elem4(k,1)+3*Nx+2;
end
for k=Nx+1:Nx*Ny
    elem4(k,1:4)=elem4(k-Nx,1:4)+3*Nx+2;
end
elem8=zeros(Nx*Ny,8);
for k=1:Nx
    elem8(k,1)=2*k-1;
    elem8(k,2)=2*k;
    elem8(k,3)=2*k+1;
    elem8(k,8)=elem8(k,1)+2*(Nx+1)-k;
    elem8(k,5)=elem8(k,3)+3*Nx+2;
    elem8(k,6)=elem8(k,2)+3*Nx+2;
    elem8(k,7)=elem8(k,1)+3*Nx+2;
    elem8(k,4)=elem8(k,8)+1;
end
for k=Nx+1:Nx*Ny
    elem8(k,1:8)=elem8(k-Nx,1:8)+3*Nx+2;
end
celem=[elem8(:,:) elem4(:,:)];
end
