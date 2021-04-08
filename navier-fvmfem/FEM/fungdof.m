function gdof=fungdof(Nx,Ny,x0,xf,y0,yf)
mnd=(Nx+1)*Ny+Nx*(Ny+1);
vnd=(Nx+1)*(Ny+1);
mnd=(Nx+1)*Ny+Nx*(Ny+1);
NN=vnd+mnd;
celem=connective(Nx,Ny);
elem8=[celem(:,1:8)];
pnode=zeros(Nx*Ny,4);
for k=1:Nx
    pnode(k,1)=NN+k;
    pnode(k,2)=NN+k+1;
    pnode(k,3)=pnode(k,2)+Nx+1;
    pnode(k,4)=pnode(k,3)-1;
end
for k=Nx+1:Nx*Ny
    pnode(k,1:4)=pnode(k-Nx,1:4)+Nx+1;
end
for e=1:length(celem(:,1))
    dofCounter=0;
    % u velocity DOFs
    for i=1:8
        dofCounter = dofCounter+1;
        gdof(e,dofCounter)=elem8(e,i);
    end
    % pressure DOFs
    for i=1:4
        dofCounter= dofCounter+1;
        gdof(e,dofCounter)=pnode(e,i);
    end
    
    %v velocity DOFs
    for i=1:8
        dofCounter=dofCounter+1;
        gdof(e,dofCounter)=2*NN-mnd+elem8(e,i);
    end
    
end
